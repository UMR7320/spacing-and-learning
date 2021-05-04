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

app.ports.playAudio.subscribe((audio) => {
  var sound = new Howl({
    src: [audio]
    
  });
  sound.play();
  sound.once('play', () => {
    let audioCallback = { "eventType": "SoundStarted","timestamp": Date.now(), "name": audio }
    app.ports.audioEnded.send(audioCallback);
    console.log({ "sound started": audio, timestamp: Date.now() })
  });
  sound.once('end', () => {
    let audioCallback = {"eventType": "SoundEnded", timestamp:Date.now(), "name":audio}
    app.ports.audioEnded.send(audioCallback);
    console.log({'sound is over':audio, timestamp: Date.now()});
    console.log(audio);
  } )
})


