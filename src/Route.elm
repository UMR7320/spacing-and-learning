module Route exposing
    ( AcceptabilityRoute(..)
    , Group(..)
    , PretestRoute(..)
    , Route(..)
    , Session1Activity(..)
    , Session2Activity(..)
    , Session3Activity(..)
    , VKSRoute(..)
    , YesNoRoute(..)
    , fromUrl
    )

import Pretest.Version exposing (Version(..))
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
    | WordCloud1


type Session2Activity
    = TopSession2
    | Meaning2
    | Spelling2
    | Context2
    | WordCloud2


type Session3Activity
    = TopSession3
    | Meaning3
    | Spelling3
    | Context3
    | WordCloud3


type PretestRoute
    = TopPretest
    | GeneralInfos
    | EmailSent
    | SPR
    | SentenceCompletion
    | VKS VKSRoute
    | Acceptability AcceptabilityRoute
    | YesNo YesNoRoute
    | Calendar Bool Group


type Group
    = Massed
    | Distributed


type YesNoRoute
    = YesNoVideo
    | YesNoTrainingInstructions
    | YesNoActivity


type VKSRoute
    = VKSVideo
    | VKSTrainingInstructions
    | VKSActivity


type AcceptabilityRoute
    = AcceptabilityInstructions
    | AcceptabilityStart
    | AcceptabilityEnd


parser : Parser (Route -> a) a
parser =
    oneOf
        -- [ map Home top
        [ map UserCode (top <?> Query.string "date")
        , map TermsAndConditions (s "terms-and-conditions")
        , map CalendarUpdated (s "calendar-updated")
        , map Pretest (s "user" </> string </> s "pretest" </> pretestRouteParser)
        , map (\userId ->\route -> Pretest userId route PostTestDiff)
            (s "user"
                </> string
                </> s "post-test-diff"
                </> oneOf
                        [ map TopPretest top
                        , map VKS vksParser
                        ]
            )
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
                        , map WordCloud1 (s "wordcloud")
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
                        , map WordCloud2 (s "wordcloud")
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
                        , map WordCloud3 (s "wordcloud")
                        ]
            )
        , map UserCode (s "code" <?> Query.string "date")

        --  Add more routes like this:
        --  , map Comment (s "user" </> string </> s "comment" </> int)
        --  , map BlogQuery (s "blog" <?> Query.string "q")
        --  Learn more: https://guide.elm-lang.org/webapps/url_parsing.html
        ]


vksParser : Parser (VKSRoute -> a) a
vksParser =
    s "vks"
        </> oneOf
                [ map VKSActivity top
                , map VKSVideo (s "video")
                , map VKSTrainingInstructions (s "instructions")
                ]


pretestRouteParser : Parser (PretestRoute -> Version -> b) b
pretestRouteParser =
    oneOf
        [ map TopPretest top
        , map SPR (s "spr")
        , map GeneralInfos (s "informations")
        , map EmailSent (s "email-sent")
        , map SentenceCompletion (s "sentence-completion")
        , map VKS vksParser
        , map YesNo
            (s "yes-no"
                </> oneOf
                        [ map YesNoActivity top
                        , map YesNoVideo (s "video")
                        , map YesNoTrainingInstructions (s "instructions")
                        ]
            )
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
