module Facility exposing (Facility, Participant, algo, isUnhappy, pushFacility, putInFacility, filterUnhappyParticipants)


type alias Facility =
    { name : String
    , size : Int
    , participants : List Participant
    }


type alias Participant =
    { name : String
    , likes : List String
    , hates : List String
    }


algo : List Participant -> List Facility -> List Facility
algo participants facilities =
    let
        unhappy =
            filterUnhappyParticipants participants facilities
    in
    List.map (pushFacility unhappy) facilities


pushFacility : List Participant -> Facility -> Facility
pushFacility participants facility =
    List.foldl putInFacility facility participants


putInFacility : Participant -> Facility -> Facility
putInFacility p f =
    if List.length f.participants >= f.size then
        f

    else if List.member f.name p.likes then
        { f | participants = p :: f.participants }

    else
        f


filterUnhappyParticipants : List Participant -> List Facility -> List Participant
filterUnhappyParticipants participants facilities =
    List.filter (isUnhappy facilities) participants


isUnhappy : List Facility -> Participant -> Bool
isUnhappy facilities participant =
    not (List.any (\f -> List.member participant f.participants) facilities)
