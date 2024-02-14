module Route exposing
    ( AcceptabilityRoute(..)
    , Group(..)
    , PosttestActivity(..)
    , PretestRoute(..)
    , Route(..)
    , Session1Activity(..)
    , Session2Activity(..)
    , Session3Activity(..)
    , fromUrl
    )

import Pretest.Version exposing (Version)
import Url
import Url.Parser as Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        , map
        , oneOf
        , s
        , string
        , top
        )
import Url.Parser.Query as Query


type Route
    = Home
    | Pretest UserId PretestRoute Version
    | Session1 UserId Session1Activity
    | Session2 UserId Session2Activity
    | Session3 UserId Session3Activity
    | Posttest UserId PosttestActivity (Maybe String)
    | UserCode (Maybe String)
    | TermsAndConditions
    | CalendarUpdated
    | NotFound


type alias UserId =
    String


type Session1Activity
    = TopSession1
    | Presentation
    | Meaning1
    | Spelling1
    | Context1


type Session2Activity
    = TopSession2
    | Meaning2
    | Spelling2
    | Context2


type Session3Activity
    = TopSession3
    | Meaning3
    | Spelling3
    | Context3


type PretestRoute
    = TopPretest
    | GeneralInfos
    | EmailSent
    | SPR
    | SentenceCompletion
    | VKS
    | Acceptability AcceptabilityRoute
    | YesNo
    | Calendar Bool Group


type Group
    = Massed
    | Distributed


type AcceptabilityRoute
    = AcceptabilityInstructions
    | AcceptabilityStart
    | AcceptabilityEnd


type PosttestActivity
    = CloudWords


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map TermsAndConditions (s "terms-and-conditions")
        , map CalendarUpdated (s "calendar-updated")
        , map Pretest (s "user" </> string </> s "pretest" </> pretestRouteParser)
        , map Session1
            (s "user"
                </> string
                </> s "session1"
                </> oneOf
                        [ map TopSession1 top
                        , map Presentation (s "presentation")
                        , map Meaning1 (s "meaning")
                        , map Spelling1 (s "spelling")
                        , map Context1 (s "context-understanding")
                        ]
            )
        , map Session2
            (s "user"
                </> string
                </> s "session2"
                </> oneOf
                        [ map TopSession2 top
                        , map Meaning2 (s "meaning")
                        , map Spelling2 (s "spelling")
                        , map Context2 (s "context-understanding")
                        ]
            )
        , map Session3
            (s "user"
                </> string
                </> s "session3"
                </> oneOf
                        [ map TopSession3 top
                        , map Meaning3 (s "meaning")
                        , map Spelling3 (s "spelling")
                        , map Context3 (s "context-understanding")
                        ]
            )
        , map Posttest
            (s "user"
                </> string
                </> s "post-tests"
                </> oneOf [ map CloudWords (s "cw") ]
                <?> Query.string "session"
            )
        , map UserCode (s "code" <?> Query.string "date")

        --  Add more routes like this:
        --  , map Comment (s "user" </> string </> s "comment" </> int)
        --  , map BlogQuery (s "blog" <?> Query.string "q")
        --  Learn more: https://guide.elm-lang.org/webapps/url_parsing.html
        ]


pretestRouteParser : Parser (PretestRoute -> Version -> b) b
pretestRouteParser =
    oneOf
        [ map TopPretest top
        , map SPR (s "spr")
        , map GeneralInfos (s "informations")
        , map EmailSent (s "email-sent")
        , map SentenceCompletion (s "sentence-completion")
        , map VKS (s "vks")
        , map YesNo (s "yes-no")
        , map (Calendar False) (s "calendar" </> oneOf [ map Massed (s "m"), map Distributed (s "d") ])
        , map (Calendar True) (s "update-calendar" </> oneOf [ map Massed (s "m"), map Distributed (s "d") ])
        , map Acceptability
            (s "acceptability"
                </> oneOf
                        [ map AcceptabilityInstructions (s "instructions")
                        , map AcceptabilityStart (s "start")
                        , map AcceptabilityEnd (s "end")
                        ]
            )
        ]
        <?> Query.map (Maybe.withDefault "pre" >> Pretest.Version.fromString) (Query.string "version")



--translationPath : Parser (String -> a) a
--userAndTask : a -> b -> c -> d -> e -> f -> g -> h -> i -> Url.Url -> Route
--spacing-and-learning.netlify.app/user/userId/task/scrabble


fromUrl : Url.Url -> Route
fromUrl url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound
