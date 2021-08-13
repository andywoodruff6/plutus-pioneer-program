{-# LANGUAGE OverloadedStrings #-}
import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract "User1" "User2" "User3" $ Constant 100
-- The quoted names are what will appear in the UI
-- the Constant 100 is the deposit amount

choiceId :: Party -> ChoiceId
choiceId p = ChoiceId "Winner" p                         -- The p is the variable for the party name

contract :: Party -> Party -> Party -> Value -> Contract -- Takes three Users and a value to return a contract
contract alice bob charlie deposit =                     -- Sets up the variable names to be used in the rest of the contract
    When
        [ f alice bob charlie                            -- Given that we are using [] in a when then it has to be a case
        , f bob alice charlie]
        20 Close
  where
    f :: Party -> Party -> Party -> Case
    f x y z =
        Case
            (Deposit z z ada $ AddValue deposit deposit)
            (When
                [Case
                    (Deposit x x ada deposit)                    -- to account, from party, token, value
                    (When
                        [Case
                            (Deposit y y ada deposit)            -- to account, from party, token, value
                            (When
                                [Case
                                    (Choice (choiceId charlie) [Bound 1 2])
                                    (If (ValueEQ (ChoiceValue $ choiceId charlie) (Constant 1))
                                        (Pay bob (Account alice) ada deposit Close)
                                        (Pay alice (Account bob) ada deposit Close)
                                    )
                                ]
                            40 
                            (Pay charlie (Account alice) ada deposit $
                             Pay charlie (Account bob) ada deposit
                            Close)                             -- Sets final timeout
                            )
                        ]
                    30 Close
                    )
                ]
            20 Close
            )