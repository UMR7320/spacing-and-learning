module Route exposing
    ( AcceptabilityRoute(..)
    , Group(..)
    , PosttestTask(..)
    , Pretest(..)
    , Route(..)
    , Session1Task(..)
    , Session2Task(..)
    , Session3Task(..)
    , fromUrl
    )

import Url
import Url.Parser as Parser
    exposing
        ( (</>)
        , Parser
        , map
        , oneOf
        , s
        , string
        , top
        )


type Route
    = Home
    | NotFound
    | Pretest UserId Pretest
    | Session1 UserId Session1Task
    | AuthenticatedSession2 UserId Session2Task
    | AuthenticatedSession3 UserId Session3Task
    | Posttest PosttestTask


type alias UserId =
    String


type Session1Task
    = Meaning
    | SpellingLevel1
    | Presentation
    | CU1
    | TopSession1


type Session2Task
    = Translation
    | Spelling
    | CU
    | TopSession2


type Pretest
    = GeneralInfos
    | EmailSent
    | SPR
    | SentenceCompletion
    | VKS
    | Acceptability AcceptabilityRoute
    | YesNo
    | Calendar Group


type Group
    = Massed
    | Distributed


type AcceptabilityRoute
    = AcceptabilityInstructions
    | AcceptabilityStart
    | AcceptabilityEnd



--| Acceptability


type PosttestTask
    = CloudWords


type Session3Task
    = CU3
    | Spelling3
    | Synonym
    | TopSession3


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Pretest
            (s "user"
                </> string
                </> s "pretest"
                </> oneOf
                        [ map SPR (s "spr")
                        , map GeneralInfos (s "informations")
                        , map EmailSent (s "email-sent")
                        , map SentenceCompletion (s "sentence-completion")
                        , map VKS (s "vks")
                        , map YesNo (s "yes-no")
                        , map Calendar (s "calendar" </> oneOf [ map Massed (s "m"), map Distributed (s "d") ])
                        , map Acceptability
                            (s "acceptability"
                                </> oneOf
                                        [ map AcceptabilityInstructions (s "instructions")
                                        , map AcceptabilityStart (s "start")
                                        , map AcceptabilityEnd (s "end")
                                        ]
                            )
                        ]
            )
        , map Session1
            (s "user"
                </> string
                </> s "session1"
                </> oneOf
                        [ map Meaning (s "meaning")
                        , map SpellingLevel1 (s "spelling")
                        , map Presentation (s "presentation")
                        , map CU1 (s "context-understanding")
                        , map TopSession1 top
                        ]
            )
        , map AuthenticatedSession2
            (s "user"
                </> string
                </> s "session2"
                </> oneOf
                        [ map Spelling (s "spelling")
                        , map Translation (s "translation")
                        , map CU (s "context-understanding")
                        , map TopSession2 top
                        ]
            )
        , map AuthenticatedSession3
            (s "user"
                </> string
                </> s "session3"
                </> oneOf
                        [ map CU3 (s "context-understanding")
                        , map Spelling3 (s "spelling")
                        , map Synonym (s "synonym")
                        , map TopSession3 top
                        ]
            )

        --  Add more routes like this:
        --  , map Comment (s "user" </> string </> s "comment" </> int)
        --  , map BlogQuery (s "blog" <?> Query.string "q")
        --  Learn more: https://guide.elm-lang.org/webapps/url_parsing.html
        ]



--translationPath : Parser (String -> a) a
--userAndTask : a -> b -> c -> d -> e -> f -> g -> h -> i -> Url.Url -> Route
--spacing-and-learning.netlify.app/user/userId/task/scrabble


fromUrl : Url.Url -> Route
fromUrl url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound
