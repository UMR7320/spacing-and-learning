import { Elm } from "./src/Main.elm";
import "./scss/style.scss";
import { Howl, Howler } from 'howler'

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}

const flags = {};

const app = Elm.Main.init({ flags });

let beepUrl = require('./sounds/beep.mp3');

function addEvent(elm, evType, fn, useCapture) {
  if (elm.addEventListener) {
    elm.addEventListener(evType, fn, useCapture);
    return true;
  }
  else if (elm.attachEvent) {
    var r = elm.attachEvent('on' + evType, fn);
    return r;
  }
  else {
    elm['on' + evType] = fn;
  }
}

function exitAlert(e) {
  var msg = "Voulez-vous vraiment quitter cette page? Des données pourraient être perdues si l'expérience n'est pas terminée.";
  if (!e) { e = window.event; }
  if (e) { e.returnValue = msg; }
  return msg;
}

addEvent(window, 'beforeunload', exitAlert, false);

app.ports.playAudio.subscribe((audio) => {
  var sound = new Howl({
    src: [audio]

  });
  sound.play();
  sound.once('play', () => {
    let audioCallback = { "eventType": "SoundStarted", "timestamp": Date.now(), "name": audio }
    app.ports.audioEnded.send(audioCallback);
    console.log({ "sound started": audio, timestamp: Date.now() })
  });
  sound.once('end', () => {
    let audioCallback = { "eventType": "SoundEnded", timestamp: Date.now(), "name": audio }
    app.ports.audioEnded.send(audioCallback);
    console.log({ 'sound is over': audio, timestamp: Date.now() });
    console.log(audio);
  })
})


