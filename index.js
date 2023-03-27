import { Elm } from "./src/Main.elm";
import "./scss/style.scss";

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}

const flags = process.env.BACKGROUND_QUESTIONNAIRE_URL;

const app = Elm.Main.init({ flags });

function exitAlert(e) {
  var msg =
    "Voulez-vous vraiment quitter cette page? Des données pourraient être perdues si l'expérience n'est pas terminée.";
  if (!e) {
    e = window.event;
  }
  if (e) {
    e.returnValue = msg;
  }
  return msg;
}

app.ports.enableAlertOnExit.subscribe(() => {
  window.onbeforeunload = exitAlert;
});

app.ports.disableAlertOnExit.subscribe(() => {
  window.onbeforeunload = null;
});

app.ports.playAudio.subscribe(audio => {
  const sound = new Audio(audio);
  sound.play();
  sound.addEventListener("play", () => {
    let audioCallback = {
      eventType: "SoundStarted",
      timestamp: Date.now(),
      name: audio
    };
    app.ports.audioEnded.send(audioCallback);
    console.log({ "sound started": audio, timestamp: Date.now() });
  }, { once: true });
  sound.addEventListener("ended", () => {
    let audioCallback = {
      eventType: "SoundEnded",
      timestamp: Date.now(),
      name: audio
    };
    app.ports.audioEnded.send(audioCallback);
    console.log({ "sound is over": audio, timestamp: Date.now() });
  }, { once: true });
});
