% Yol ağı tanımlamaları
edge(admin_Office, cafeteria, 4).
edge(cafeteria, admin_Office, 4).
edge(admin_Office, library, 1).
edge(library, admin_Office, 1).
edge(admin_Office, engineering_Bld, 3).
edge(engineering_Bld, admin_Office, 3).
edge(cafeteria, library, 5).
edge(library, cafeteria, 5).
edge(cafeteria, social_Sciences_Bld, 2).
edge(social_Sciences_Bld, cafeteria, 2).
edge(engineering_Bld, library, 5).
edge(library, engineering_Bld, 5).
edge(engineering_Bld, lecture_hall_a, 2).
edge(lecture_hall_a, engineering_Bld, 2).
edge(library, social_Sciences_Bld, 2).
edge(social_Sciences_Bld, library, 2).
edge(library, institute_y, 3).
edge(institute_y, library, 3).
edge(social_Sciences_Bld, institute_x, 8).
edge(institute_x, social_Sciences_Bld, 8).
edge(lecture_hall_a, institute_y, 3).
edge(institute_y, lecture_hall_a, 3).

% Facts about delivery personnel
% id,kgiçalışma saati,şuanda çalıştığı teslimat işi, şuanki konumuu
delivery_personnel(1, 20, 16, none, institute_y).
delivery_personnel(2, 20, 12, none, engineering_Bld).
delivery_personnel(3, 25, 20 , none, institute_x).
delivery_personnel(4, 25, 12, carry, social_Sciences_Bld).

% Facts about objects
% id,kgialınacağı yer,teslim edileceği yer, teslimatın aciliyeti, eğer taşımadaysa kim taşiyor.

object(1, 5, institute_y, cafeteria, low, none).
object(2, 8, admin_Office,institute_x, medium, none).
object(3, 3, cafeteria, library, high, move). % Object 3 is currently in transit with delivery person 1
object(4, 10, lecture_hall_a , cafeteria, medium, none).
object(5, 6, library, admin_Office, low, none).

% Dijkstra algoritması
% En kısa mesafeyi bulma fonksiyonu
shortest_path(Start, End, Path, DistanceTime) :-
    dijkstra([(0, Start, [])], End, Path, DistanceTime).

dijkstra([(DistanceTime, Node, CurrentPath) | Rest], End, Path, DistanceTime) :-
    Node == End,
    reverse([Node | CurrentPath], Path).


dijkstra([(CurrentDistance, CurrentNode, CurrentPath) | Rest], End, Path, DistanceTime) :-
    CurrentNode \== End, % En kısa yol bulunmadıysa devam eder
    findall((NewDistance, Neighbor, [CurrentNode | CurrentPath]),
            (edge(CurrentNode, Neighbor, EdgeDistance),
             NewDistance is CurrentDistance + EdgeDistance,
             \+ member(Neighbor, CurrentPath)),
            Neighbors),
    append(Rest, Neighbors, UpdatedQueue),
    sort(UpdatedQueue, SortedQueue),
    dijkstra(SortedQueue, End, Path, DistanceTime).

% Calculate the distance between two locations
distanceTime(Location1, Location2, DistanceTime) :-
    shortest_path(Location1, Location2, _, DistanceTime).
    
% Kargonun taşınabileceği personelleri bulma
find_personnel_and_time(CargoID, PersonnelID, TotalTime) :-
    object(CargoID, Weight, Pickup, Delivery, _, Status),
    delivery_personnel(PersonnelID, Capacity, TimeWork, Status, Location),
    Weight =< Capacity,
    distanceTime(Location, Pickup,  Time1),
    distanceTime(Pickup, Delivery, Time2),
    TotalTime is Time1 + Time2,
    TotalTime =< TimeWork,
    Status == none.

% Kargonun taşınabileceği tüm personelleri ve en kısa süreleri listeleme
list_personnel_for_cargo(CargoID) :-
    object(CargoID, Weight, Pickup, Delivery, _, Status),
    (Status == none ->
        findall((PersonnelID, TotalTime),
                find_personnel_and_time(CargoID, PersonnelID, TotalTime),
                PersonnelList),
        write('Personnel and times for which cargo can be transported:\n'),
        print_personnel_list(PersonnelList)
    ;
        delivery_personnel(ClosestPersonnelId, _, _, carry, _),
        write('Cargo is currently being transported PERSONEL ID: '), write(ClosestPersonnelId), nl
    ).
% Personel listesini yazdırma yardımcı kuralı
print_personnel_list([]).
print_personnel_list([(PersonnelID, TotalTime) | Rest]) :-
    write('Personel ID: '), write(PersonnelID),
    write(', Toteltime: '), write(TotalTime), write(' hour\n'),
    print_personnel_list(Rest).



?- list_personnel_for_cargo(1). 
