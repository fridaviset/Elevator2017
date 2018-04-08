-module(order_distributer).
-export([start/0]).

start()->
    timer:sleep(200),
    loop().

loop()->
    receive
        {new_order, OrderDirection, OrderFloor}->
            worldview_pid ! {add_order, node(), OrderDirection, OrderFloor},
            schedule_pid ! {new_order, OrderDirection, OrderFloor},
            loop();
        {order_served, cab, OrderFloor}->
            worldview_pid ! {remove_order, node(), cab, OrderFloor},
            loop();
        {order_served, OrderDirection, OrderFloor}->
            worldview_pid ! {remove_order, node(), OrderDirection, OrderFloor},
            %Add functionality to update worldview of other elevators here!
            loop()

    end.