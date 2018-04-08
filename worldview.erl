-module(worldview).
-export([start/0, print_worldview/0, print_elevator_status/1]).

-record(order, {floor, direction}).
-record(orderlist, {elevatorname, orders = []}).

%API

print_worldview()-> worldview_pid ! {print_worldview}.

%"WorldView" is a list of all elevators schedules in network

start()->
    timer:sleep(400),
    WorldView=[#orderlist{elevatorname=node()}],
    %Add functionality to go through lists of nodes to add them to worldview
    loop(WorldView).

loop(WorldView)->
    receive 
        {print_worldview} ->
            print_worldview(WorldView),
            loop(WorldView);
        {add_order, ElevatorName, Direction, Floor} ->
            NewWorldView=add_order(ElevatorName,Floor,Direction,WorldView),
            print_worldview(WorldView),
            loop(NewWorldView);
        {remove_order, ElevatorName, Direction, Floor}->
            NewWorldView=remove_order(ElevatorName,Floor,Direction,WorldView),
            %print_worldview(WorldView),
            loop(NewWorldView);
        {is_order, Caller, Direction, Floor}->
            Elevatorname=is_order(Floor, Direction,WorldView),
            case Elevatorname of
                none ->
                    Caller ! {no_elevators_have_this_order};
                _->
                    Caller ! {elev_has_this_order, Elevatorname}
            end,
            loop(WorldView)
    end.


is_order(Floor,Direction,WorldView)->
    Order=#order{floor=Floor,direction=Direction},
    case WorldView =:= [] of
        true ->
            none;
        false ->
            [Head|Tail]=WorldView,
                case lists:member(Order, Head#orderlist.orders) of 
                    true ->
                        Head#orderlist.elevatorname;
                    false ->
                        is_order(Floor,Direction,Tail)
                end
    end.

add_order(ElevatorName, Floor, Direction, WorldView)->
    add_order(ElevatorName, Floor, Direction, [], WorldView).

add_order(ElevatorName, Floor, Direction, WorldView, Tail)->
    Order=#order{floor=Floor,direction=Direction},
    case Tail =:= [] of
        true ->
            WorldView;
        false ->
            [Head|NewTail]=Tail,
            case ((ElevatorName =:= Head#orderlist.elevatorname) and (not (lists:member(Order, Head#orderlist.orders)))) of 
                true ->
                    NewOrderList=Head#orderlist{orders=[Order|Head#orderlist.orders]},
                    NewWorldView=[NewOrderList|WorldView],
                    add_order(ElevatorName,Floor,Direction,NewWorldView,NewTail);
                false ->
                    NewWorldView=[Head|WorldView],
                    add_order(ElevatorName,Floor,Direction,NewWorldView,NewTail)
                end
end.

remove_order(ElevatorName, Floor, Direction, WorldView)->
        remove_order(ElevatorName, Floor, Direction, [], WorldView).

remove_order(ElevatorName, Floor, Direction, WorldView, Tail)->
    Order=#order{floor=Floor,direction=Direction},
    case Tail =:= [] of
        true ->
            WorldView;
        false ->
            [Head|NewTail]=Tail,
                case ((ElevatorName =:= Head#orderlist.elevatorname) and (lists:member(Order, Head#orderlist.orders))) of 
                    true ->
                        NewWorldView=[Head#orderlist{orders=lists:delete(Order,Head#orderlist.orders)}|WorldView],
                        remove_order(ElevatorName,Floor,Direction,NewWorldView,NewTail);
                    false ->
                        NewWorldView=[Head|WorldView],
                        remove_order(ElevatorName,Floor,Direction,NewWorldView,NewTail)
                end
end.

print_elevator_status(Elem)->
    io:format("~p",[Elem#orderlist.elevatorname]),
    print_list_of_orders(Elem#orderlist.orders).

print_worldview(WorldView)->
    case WorldView of
        []->
            io:format("worldview is empty~n");
        _->
            io:format("worldview: "),
            lists:foreach(fun(Elem)-> print_elevator_status(Elem) end,WorldView)
    end.

print_list_of_orders(List)->
    case List of
        [] ->
            ok;
        [Head|Tail]->
            print_order1(Head#order.direction,Head#order.floor),
            print_list_of_orders(Tail)
    end.

print_order1(Direction, Floor) ->
    case Direction of
        cab -> io:format("cab, ~b~n ", [Floor]);
        hall_up -> io:format("hall up, ~b~n ", [Floor]);
        hall_down -> io:format("hall down, ~b~n ", [Floor])
    end.