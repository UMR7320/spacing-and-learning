# spacing-and-learning

Lexlearn (aka spacing-and-learning dans ce repo) est une expérience visant à mieux comprendre l'effet d'espacement sur l'apprentissage de nouvelles structures syntaxiques en anglais par des locuteurs non-natifs.

## Faire tourner l'application en local

```bash
git clone https://gitlab.com/YoelisA/spacing-and-learning.git
#Add Netlify_AUTH_TOKEN to PATH
echo 'export NETLFIY_AUTH_TOKEN=TokenDonnéParYoelisOuLaurent' > .zprofile
source .zprofile
#Vérifier que l'environnement variable est bien ajoutée au PATH
printenv NETLIFY_AUTH_TOKEN
cd spacing-and-learning
npm install
npm run dev
npm run test:watch
```

## La stack

Le front-end de cette application a été créé avec [ Elm ](https://elm-lang.org  "A delightful language"). Si vous n'êtes pas familier avec ce langage ou au moins avec les fondamentaux de la programmation fonctionnelle, je vous recommande de lire [ce guide](https://guide.elm-lang.org) avant de vous aventurer davantage dans ce repo'. Le style est géré par [tailwindcss](https://tailwindcss.com).

La base de donnée est construite avec [airtable](https://airtable.com/invite/l?inviteId=invpVjhEIAAz11jXh&inviteToken=04f15c7eb855d113135e28d956208b2631512b7ea1b85fd3b845e974b3db693b). Cela vaut le coup de jeter un oeil à la façon dont tout est architecturé au niveau de la base de données avant d'explorer le code. Cette base de donnée est aussi l'interface d'administration permettant aux chercheuses d'éditer un certain nombre de paramètres.

Lexlearn est composée de plusieurs `sessions`, chaque session est composée d'une ou plusieurs `tasks`.

Dans `input`on trouvera tout le matériel relatif aux tâches qui appartiennent aux sessions 1, 2 et 3. Le matériel des autres tâches se trouve dans les tables `sentence_completion` et `SPR`.

Toutes les données générées sont envoyées dans la table `output`.

`users`référence toutes les informations relatives aux participants de cette étude.

La communication entre l'application et la base de donnée est gérée par une API serverless écrite en javascript déployée sous forme d'une [lambda function](https://www.netlify.com/products/functions/).

## La structure des dossiers

   - __img__ -> Image assets
   - [index.html](index.html) -> Le point d'entrée de l'application
   - [index.js](index.js) -> C'est ici que se trouve les ports permettant de communiquer avec JS
   - [netlify.toml](netlify.toml) -> Les instructions pour netlify
   - [package.json](package.json) -> Modules
   - [postcss.config.js](postcss.config.js) -> Config postcss générée par elm-batteries
   - __scss__
     - [style.scss](scss/style.scss)
   - __sounds__ -> Sounds assets
   - __src__ -> C'est ici que tout se passe !
     - [Data.elm](src/Data.elm) -> On trouvera les fonctions permettant de recevoir ou d'envoyer des données
     - [ExperimentInfo.elm](src/ExperimentInfo.elm) -> Fonctions permettant d'obtenir les informations relatives à chaque tâche.
     - [Icons.elm](src/Icons.elm) -> Quelques îcones svg
     - [Logic.elm](src/Logic.elm) -> Toute la logique pour chaque tâche se trouve ici.
     - [Main.elm](src/Main.elm) -> Ici convergent tous les modules.
     - [Ports.elm](src/Ports.elm) -> Les fonctions permettant d'interagir avec JS se trouvent ici.
     - __Postest__ -> Les tâches se trouvant dans les post-tests.
       - [CloudWords.elm](src/Postest/CloudWords.elm)
       - [YN.elm](src/Postest/YN.elm)
     - __Pretest__ -> Les tâches prétextes
       - [Acceptability.elm](src/Pretest/Acceptability.elm)
       - [GeneralInfos.elm](src/Pretest/GeneralInfos.elm)
       - [Pretest.elm](src/Pretest/Pretest.elm)
       - [SPR.elm](src/Pretest/SPR.elm)
       - [SentenceCompletion.elm](src/Pretest/SentenceCompletion.elm)
       - [VKS.elm](src/Pretest/VKS.elm)
     - [ProgressBar.elm](src/ProgressBar.elm) -> Expose une barre de progrès
     - [Route.elm](src/Route.elm) -> C'est ici que le routing est géré !
     - [Session.elm](src/Session.elm) -> Fonctions pour décoder les informatins relatives aux sessions
     - __Session1__ -> Les tâches de la session 1
       - [ContextUnderstanding.elm](src/Session1/ContextUnderstanding.elm)
       - [Meaning.elm](src/Session1/Meaning.elm)
       - [Presentation.elm](src/Session1/Presentation.elm)
       - [Session.elm](src/Session1/Session.elm)
       - [Spelling.elm](src/Session1/Spelling.elm)
       - [Top.elm](src/Session1/Top.elm)
     - __Session2__ -> Les tâches de la session 2
       - [CU2.elm](src/Session2/CU2.elm)
       - [Session.elm](src/Session2/Session.elm)
       - [Spelling.elm](src/Session2/Spelling.elm)
       - [Translation.elm](src/Session2/Translation.elm)
     - __Session3 __-> Les tâches de la session 3
       - [CU3.elm](src/Session3/CU3.elm)
       - [Session.elm](src/Session3/Session.elm)
       - [Spelling3.elm](src/Session3/Spelling3.elm)
       - [Synonym.elm](src/Session3/Synonym.elm)
     - [User.elm](src/User.elm) -> Fonctions relatives aux utilisateurs
     - [View.elm ](src/View.elm)-> Éléments UI utilisés dans toutes les tâches
   - [tailwind.config.js ](tailwind.config.js) -> Config tailwinds
   - __tests__ -> Ici se trouve les tests
     - [Example.elm](tests/Example.elm)





## Author

- Yoelis Acourt

