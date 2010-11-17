% Declaring a record representing robots with 4 fields.
-record(robot, {name,
                type=industrial, % Default value
                hobbies,
                details=[]}).
