iris(SL, SW, PL, PW, Type) :-
    (PW =< 0.6 ->
        Type = 'Iris-Setosa'
    ;
        (PL =< 4.75 ->
            (PW =< 1.65 ->
                Type = 'Iris-Versicolor'
            ;
             (SL > 7.0 ->
             Type = 'Iris-Virginica'
             ;
             Type = 'Iris-Virginica'
             )
            )
        ;
            (PW =< 1.75 ->
                (PL =< 4.95 ->
                    Type = 'Iris-Versicolor'
                ;
                    (PW =< 1.55 ->
                        (SL > 7.0 ->
                       Type = 'Iris-Virginica'
                         ;
                       Type = 'Iris-Virginica'
                        )
                    ;
                        (SL =< 6.95 ->
                            Type = 'Iris-Versicolor'
                        ;
                            Type = 'Iris-Virginica'
                        )
                    )
                )
            ;
                (PL =< 4.85 ->
                    (SW =< 3.1 ->
                        Type = 'Iris-Virginica'
                    ;
                       (SW =< 2.2 ->
                       Type = 'Iris-Versicolor'
                        ;
                       Type = 'Iris-Versicolor'
                       )
                    )
                ;
                    Type = 'Iris-Virginica'
                )
            )
        )
    ).


?- iris(7.6,3.0,6.6,2.1,Type).
?- iris(4.9,2.5,4.5,1.7,Type).
?- iris(7.0,3.2,4.7,1.4,Type).
?- iris(4.6,3.1,1.5,0.2,Type).