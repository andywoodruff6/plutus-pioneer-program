{-# LANGUAGE OverloadedStrings #-}
import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract "Andy" "Ben" "Connor" $ Constant 100

choiceId :: Party -> ChoiceId
choiceId p = ChoiceId "Winner" p

contract :: Party -> Party -> Party -> Value -> Contract
contract person_one person_two person_three deposit =
    When
        [ f person_one person_two
        , f person_two person_one
        ]
        10 Close
  where
    f :: Party -> Party -> Case
    f x y =
        Case
            -- game host to post collateral
            (Deposit
                person_three
                person_three
                ada
                (AddValue deposit deposit)
            )
                (When
                    [Case
                        -- first person to ante up
                        (Deposit
                            x
                            x
                            ada
                            deposit
                        )
                        (When
                            [Case
                                -- second person to ante up
                                (Deposit
                                    y
                                    y
                                    ada
                                    deposit
                                )
                                (When
                                    [Case
                                        -- person_three picks a winner
                                        (Choice
                                            (choiceId person_three)
                                            [Bound 1 2]
                                        )
                                        -- IF three picks 1 give $ to one, else give $ to two
                                        (If
                                            (ValueEQ
                                                (ChoiceValue $ choiceId person_three)
                                                (Constant 1)
                                            )
                                            (Pay
                                                person_two
                                                (Account person_one)
                                                ada
                                                deposit
                                                Close
                                            )
                                            (Pay
                                                person_one
                                                (Account person_two)
                                                ada
                                                deposit
                                                Close
                                            )
                                        )]
                                    40 -- picking time out. if it triggers three loses their collateral
                                        (Pay
                                            person_three
                                            (Account person_one)
                                            ada
                                            deposit
                                            (Pay
                                                person_three
                                                (Account person_two)
                                                ada
                                                deposit
                                                Close
                                            )
                                        )
                                )]
                            30 Close
                        )]
                    20 Close
                )
            