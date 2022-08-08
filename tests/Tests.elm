module Tests exposing (tests)

import Expect
import Facility exposing (Facility, Participant, algo, filterUnhappyParticipants, pushFacility, putInFacility)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Facility Module"
        [ describe "filterParticipants"
            [ test "filter participants" <|
                \_ ->
                    let
                        tom =
                            Participant "Tom" [ "Tanzen" ] []

                        max =
                            Participant "Max" [] []

                        facilities =
                            [ Facility "Tanzen" 3 [ max ] ]
                    in
                    Expect.equal [ tom ] (filterUnhappyParticipants [ tom, max ] facilities)
            ]
        , describe "putInFacility" <|
            let
                max =
                    Participant "Max" [] []

                tom =
                    Participant "Tom" [ "Tanzen" ] []

                facility =
                    Facility "Tanzen" 2 [ max ]
            in
            [ test "facility is full" <|
                \_ ->
                    let
                        got =
                            putInFacility tom { facility | size = 1 }
                    in
                    Expect.false "Expect Tom not to be in list" (List.member tom got.participants)
            , test "facility has space" <|
                \_ ->
                    let
                        got =
                            putInFacility tom facility
                    in
                    Expect.true "Expect Tom to be in list" (List.member tom got.participants)
            , test "facility is not liked" <|
                \_ ->
                    let
                        got =
                            putInFacility { tom | likes = [ "Schwimmen" ] } facility
                    in
                    Expect.false "Expect Tom not to be in list" (List.member tom got.participants)
            ]
        , describe "pushFacility"
            [ test "run" <|
                \_ ->
                    let
                        max =
                            Participant "Max" [ "Schwimmen" ] []

                        tina =
                            Participant "Tina" [ "Schwimmen" ] []

                        rolf =
                            Participant "Rolf" [ "Tanzen" ] []

                        schwimmen =
                            Facility "Schwimmen" 3 []

                        got =
                            pushFacility [ max, tina, rolf ] schwimmen
                    in
                    Expect.all
                        [ expectMember max
                        , expectMember tina
                        , expectNoMember rolf
                        ]
                        got.participants
            ]
        , describe "algo"
            [ test "run" <|
                \_ ->
                    let
                        max =
                            Participant "Max" [ "Schwimmen", "Putzen" ] []

                        tina =
                            Participant "Tina" [ "Schwimmen" ] []

                        rolf =
                            Participant "Rolf" [ "Tanzen" ] []

                        schwimmen =
                            Facility "Schwimmen" 3 []

                        tanzen =
                            Facility "Tanzen" 3 []

                        putzen =
                            Facility "Putzen" 3 [ max ]

                        got =
                            algo [ max, tina, rolf ] [ schwimmen, tanzen, putzen ]
                    in
                    Expect.equal got [ { schwimmen | participants = [ tina ] }, { tanzen | participants = [ rolf ] }, putzen ]
            ]
        ]


expectMember : a -> List a -> Expect.Expectation
expectMember subject list =
    Expect.true "Expect it to be a list member" (List.member subject list)


expectNoMember : a -> List a -> Expect.Expectation
expectNoMember subject list =
    Expect.false "Expect it not to be a list member" (List.member subject list)
