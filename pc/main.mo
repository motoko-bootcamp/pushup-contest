import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Cycles "mo:base/ExperimentalCycles";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Random "mo:base/Random";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Map "mo:map/Map";
import { phash } "mo:map/Map";
import Sha256 "mo:sha2/Sha256";

import Hex "hex";
import Http "http";

shared ({ caller = _ }) actor class C(
    pc : Principal,
    playerName : Text,
    admin : Principal,
    wasm : Blob,
) = this {

    let hash : Text = Hex.encode(Blob.toArray(Sha256.fromBlob(#sha256, wasm)));

    private let VERSION = 1;
    private let INTERNAL_ERROR = "Critical error in Core, please report to devs @motokobootcamp";
    private let SEB_PER_BOOT = 100_000_000; // 10^8 SEB per BOOT
    private let R3_IN_CYCLES = 10_000_000_000; // 100 R3 = 1T Cycles

    class Boot() {
        var state : Core = {
            this = Principal.fromActor(this);
            name = "Players Club";
            var players = Map.new<PlayerId, Player>();
        };

        public func balance(pid : PlayerId) : Nat {
            switch (Map.get<PlayerId, Player>(state.players, phash, pid)) {
                case (null) { Debug.trap(INTERNAL_ERROR) };
                case (?player) { player.wallet };
            };
        };
        public func name() : Text { state.name };
        public func size() : Nat { state.players.size() };
        public func energy() : R3 { Cycles.balance() / R3_IN_CYCLES };

        Map.set<PlayerId, Player>(state.players, phash, Principal.fromActor(this), { name = playerName; grade = #og; pushup = energy(); wallet = SEB_PER_BOOT });

        public func players() : Map.Map<PlayerId, Player> {
            return state.players;
        };
        public func supply() : BOOT {
            var counter : Nat = 0;
            for (player in Map.vals(state.players)) {
                counter += player.wallet;
            };
            return counter / SEB_PER_BOOT;
        };
        public func price() : Price { energy() / supply() };

        public func actionToTransaction<system>(
            caller : Principal,
            action : Action,
        ) : Transaction {
            return ({
                caller;
                fee = Cycles.accept<system>(Cycles.available()) / R3_IN_CYCLES;
                action;
            });
        };

        public func inspect() : async () {
            let random = Random.Finite(await Random.blob());
            switch (random.range(16)) {
                case (null) {
                    Debug.trap(INTERNAL_ERROR);
                };
                case (?n) {
                    if (n == 0) {
                        // Find the recruit with max amount of pushup
                        var maxPushups : Nat = 0;
                        var leader : ?Player = null;
                        var leaderId : ?PlayerId = null;

                        for ((id, player) in Map.entries(state.players)) {
                            if (player.grade != #recruit and player.pushup > maxPushups) {
                                maxPushups := player.pushup;
                                leader := ?player;
                                leaderId := ?id;
                            };
                        };

                        // Promote the leader to recruit if they exist
                        switch (leader) {
                            case (null) {
                                return;
                            };
                            case (?topPlayer) {
                                switch (leaderId) {
                                    case (null) {
                                        Debug.trap(INTERNAL_ERROR);
                                    };
                                    case (?id) {
                                        // Update player's grade to recruit and reward them with 1 BOOT token
                                        let updatedPlayer = {
                                            name = topPlayer.name;
                                            grade = #recruit;
                                            pushup = topPlayer.pushup;
                                            wallet = topPlayer.wallet + SEB_PER_BOOT; // Add 1 BOOT (100M SEB)
                                        };
                                        Map.set<PlayerId, Player>(state.players, phash, id, updatedPlayer);
                                    };
                                };
                            };
                        };
                    };
                    return;
                };
            };
        };

        public func execute<system>(
            transaction : Transaction
        ) : async Result<Text, Text> {
            switch (transaction.action) {
                case (#greet(name)) {
                    if (transaction.caller == state.this) {
                        return #err("Can't move to where you are.");
                    };
                    switch (Map.get<PlayerId, Player>(state.players, phash, transaction.caller)) {
                        case (null) {
                            Map.set<PlayerId, Player>(state.players, phash, transaction.caller, { name; grade = #civil; pushup = 0; wallet = 0 });
                            return #ok("Welcome, " # name # "You are in the camp. ⛺️" # "Current mission: Complete push-ups to earn your rank, more infos @motokobootcamp.fun \n");
                        };
                        case (?_) {
                            return #ok("Nice to see you again, " # name # "You are in the camp. ⛺️" # "Current mission: Wait for Briefing info, more infos @motokobootcamp.fun \n");
                        };
                    };
                };
                case (#pushup(amount)) {
                    if (amount > transaction.fee) {
                        return #err("You call that pushups? Really? I counted only: " # Nat.toText(transaction.fee));
                    };
                    switch (Map.get<PlayerId, Player>(state.players, phash, transaction.caller)) {
                        case (null) {
                            return #err("You can't do pushup if you are not here. (move to Club first)");
                        };
                        case (?player) {
                            Map.set<PlayerId, Player>(state.players, phash, transaction.caller, { player with pushup = player.pushup + amount });
                            return #ok("Keep going! 💪");
                        };
                    };
                };
            };
        };

        public func enter() : Text {
            return (
                "Welcome to your Club" # playerName # " ." #
                "We currently have " # Nat.toText(size() - 1) # " other Players your Club." #
                "Your Balance: " # Nat.toText(balance(Principal.fromActor(this)) / SEB_PER_BOOT) # " $BOOT"
            );
        };
    };

    class Camp(
        core : Principal
    ) {
        let Club : actor {
            name : shared query () -> async Text;
            play : shared (action : Action) -> async Result.Result<Text, Text>;
        } = actor (Principal.toText(core));

        public func play(
            action : Action,
            amount : Nat,
        ) : async Result.Result<Text, Text> {
            Cycles.add<system>(amount * R3_IN_CYCLES);
            await Club.play(action);
        };

    };

    var YC : Boot = Boot(); // YC
    var PC : Camp = Camp(pc); // Players Club

    public type R3 = Nat;
    public type BOOT = Nat;
    public type SEB = Nat;
    public type Cycles = Nat;
    public type Time = Nat;
    public type Name = Text;
    public type Message = Text;
    public type Result<Ok, Err> = Result.Result<Ok, Err>;
    public type RequestId = Principal;
    public type PlayerId = Principal;
    public type Price = Nat; // Cycles per SEB
    public type Rules = Text;

    public type Core = {
        this : Principal;
        name : Text;
        var players : Map.Map<PlayerId, Player>;
    };

    public type Player = {
        name : Name;
        grade : Grade;
        pushup : R3;
        wallet : SEB;
    };

    public type Grade = {
        #og;
        #recruit;
        #civil;
    };

    public type Action = {
        #greet : Name;
        #pushup : R3;
    };

    public type Transaction = {
        caller : PlayerId;
        fee : R3;
        action : Action;
    };

    public shared ({ caller }) func enter() : async Text {
        assert (caller == admin);
        YC.enter();
    };

    public shared ({ caller }) func move() : async Result.Result<Text, Text> {
        assert (caller == admin);
        await PC.play(#greet(playerName), 1);
    };

    public shared ({ caller }) func pushup(
        amount : Nat
    ) : async Result.Result<Text, Text> {
        assert (caller == admin);
        await PC.play(#pushup(amount), amount);
    };

    public shared ({ caller }) func play(
        action : Action
    ) : async Result.Result<Text, Text> {
        let transaction = YC.actionToTransaction<system>(caller, action);
        await YC.execute(transaction);
    };

    public query func http_request(req : Http.Request) : async Http.Response {

        return ({
            status_code = 200;
            headers = [("Content-Type", "text/plain")];
            body = Text.encodeUtf8(html());
            streaming_strategy = null;
            upgrade = null;
        });
    };

    func html() : Text {
        let header = "Do you have arms strong enough to make it to the Briefing?\n\n";

        let status = "Briefing\n"
        # "==========\n"
        # "Date: ???\n"
        # "Location: ???\n"
        # "Title: ???\n"
        # "Spots: " # Nat.toText(YC.size() - 1) # "/10 Recruits\n\n";

        let rules = "Mission\n"
        # "==========\n"
        # "1. You are challenged to prove your strength, endurance and determination. It's Pushup Time!  \n"
        # "2. This mission is dead simple. More pushups = Higher rank \n"
        # "3. Captain Seb inspects the leaderboard regularly\n"
        # "4. With every inspection the one at the top of the leadeaboard has a chance of winning\n"
        # "5. Winner earn 1 $BOOT token, becomes a recruit and gets access to the Briefing date, location and title.\n"
        # "6. This mission ends when our first 10 Recruits have been selected. \n\n";

        // Display recruits first
        var recruits = "Recruits 0/10\n"
        # "==========\n";
        for (player in Map.vals(YC.players())) {
            if (player.grade == #recruit) {
                recruits #= player.name # ": " # Nat.toText(player.pushup) # " pushups\n";
            };
        };
        recruits #= "\n";

        // Display leaderboard (excluding recruits)
        var board = "Proofs-Of-Work\n"
        # "==========\n";
        for (player in Map.vals(YC.players())) {
            if (player.grade != #recruit) {
                board #= player.name # ": " # Nat.toText(player.pushup) # " pushups\n";
            };
        };
        board #= "\n";

        let dashboard = "Network\n"
        # "==========\n"
        # "You are connected to: " # (YC.name()) # " (" # playerName # ")\n"
        # "There are currently " # Nat.toText(YC.size()) # " other Player(s) in this Club! \n\n"
        # "Energy: " # Nat.toText(YC.energy()) # " R3\n"
        # "Supply: " # Nat.toText(YC.supply()) # " $BOOT\n"
        # "Price: " # Nat.toText(YC.price()) # " R3/BOOT\n\n";

        let seal = "Seal Of Trust\n"
        # "==========\n"
        # "A seal that guarantees we are Playing the same game.\n"
        # "Hash:" # hash # "\n"
        # "Source code: " # "https://github.com/motoko-bootcamp/r3boot\n";

        header # status # rules # recruits # board # dashboard # seal;
    };
};
