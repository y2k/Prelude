namespace Y2k.EventBus

type Event =
    interface
    end

type Command =
    interface
    end

type Initialize = Initialize
    with
        interface Event

type InitializeCompleted = InitializeCompleted
    with
        interface Command
