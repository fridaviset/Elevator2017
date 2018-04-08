-module(elev1).
-export([start/0]).
-define(NumberOfFloors,4).

start() ->

    %Spawning schedule and driver
    connector:start(),
    Self=self(),
    DriverPid = spawn(fun() -> driver:start(Self) end),
    register(driver_pid, DriverPid),
    SchedulePid = spawn(fun() -> schedule:start(Self) end),
    register(schedule_pid, SchedulePid),
    WorldViewPid = spawn(fun() -> worldview:start() end),
    register(worldview_pid, WorldViewPid),
    OrderDistributerPid = spawn(fun() -> order_distributer:start() end),
    register(order_distributer_pid, OrderDistributerPid),

    InitialFloor=drive_up_to_closest_floor(), %Todo: rename inital_floor inside this thing to initial_state cause this is bad
    Direction=up,
    idle(Direction,InitialFloor).

go_on(Direction, Destination, PreviousFloor) ->
    timer:sleep(50),
    receive
        {floor_reached, between_floors}->
            go_on(Direction,Destination,PreviousFloor);
        {floor_reached, Floor} ->
            hit_floor(Direction,Destination,Floor)
    end.

hit_floor(Direction,Destination,Floor)->
    case Destination =:= Floor of
        true -> 
            driver_pid ! {set_motor_direction, stop},
            served_order(Direction,Floor),
            open(Direction,Destination,Floor);
        false ->
            schedule_pid ! {is_order_in_schedule,Direction,Floor},
            io:format("fsm to schedule: Is this in schedule: "),
            print_order1(Direction,Floor),
            receive
                {order_is_in_schedule,Direction,Floor}->
                    io:format("Yes and I will open doors now~n"),
                    served_order(Direction,Floor),
                    driver_pid ! {set_motor_direction, stop},
                    open(Direction,Destination,Floor); %Here, the information about wheather this is a stop-by or reaching destination is kept.
                {order_is_not_in_schedule, Direction, Floor}->
                    io:format("Nope~n"),
                    go_on(Direction,Destination,Floor)
            end
    end.

idle(Direction, Floor)->
    timer:sleep(50),
    NewDestination = get_next_destination(Direction, Floor), %The receive-loop here is just not transparent for shit
    NewDirection = get_next_direction(Floor, NewDestination),
    case NewDestination of
        -1->
            idle(Direction, Floor);
        Floor->
            open(NewDirection, NewDestination, Floor);
        _->
            driver_pid ! {set_motor_direction, NewDirection},
            go_on(NewDirection, NewDestination, Floor)
        end.

open(Direction,Destination,Floor)-> %when you are just open, you preserve the direction you came from
    driver_pid ! {set_door_open_lamp, on},
    ProcessPid=self(),
    TimerPid=spawn(fun()-> door_open_timer(2000,ProcessPid) end),
    receive Message ->
        case Message of  
            {wait_time_over} ->
                io:format("wait_time is over wihu~n"),
                driver_pid ! {set_door_open_lamp, off},
                case Destination =:= Floor of
                        true -> 
                            idle(Direction, Floor);
                        false ->
                            driver_pid ! {set_motor_direction, Direction},
                            go_on(Direction, Destination, Floor) %This looks dangerous, why no flying elevator?
                end
                        
        end
    end.


door_open_timer(WaitTime, Listener)->
    timer:sleep(WaitTime),
    Listener ! {wait_time_over}.

drive_up_to_closest_floor()->
    receive {initial_floor, InitialFloor}->
        case InitialFloor of
            between_floors ->
                driver:set_motor_direction(up),
                receive {floor_reached, Floor} ->
                    driver:set_motor_direction(stop),
                    Floor
                end;
            FloorNumber ->
                FloorNumber
        end
    end.

get_next_direction(Floor, Destination) ->
    case Floor of
        Floor when Destination =:= -1 ->
            stop;
        Destination ->
            open;
        Floor when Floor < Destination ->
            up;
        Floor when Floor > Destination ->
            down
    end.


%%%%THis one should be part of the schedule module, this is confusing AF
get_next_destination(FromDirection, FromFloor)->
    schedule_pid ! {get_next_destination, FromDirection, FromFloor},
    receive
        {next_destination, NextDestination}->
            case NextDestination of
                -1->
                    ok;
                _->
                    io:format("Next destination is: ~b~n",[NextDestination])
            end,
            NextDestination
    end.

served_order(Direction, Floor)->
    order_distributer_pid ! {order_served, cab, Floor},
    schedule_pid ! {remove_order, cab, Floor},
    case Direction of
        up->
            order_distributer_pid ! {order_served, hall_up, Floor},
            schedule_pid ! {remove_order, hall_up, Floor};
        down->
            order_distributer_pid ! {order_served, hall_down, Floor},
            schedule_pid ! {remove_order, hall_down, Floor};
        stop->
            ok
    end.


print_order1(Direction, Floor) ->
    case Direction of
        stop -> io:format("cab, ~b~n ", [Floor]);
        up -> io:format("hall up, ~b~n ", [Floor]);
        down -> io:format("hall down, ~b~n ", [Floor])
    end.