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
  sound.on('end', () => {
    let audioCallback = {"endedAt":Date.now(), "audioName":audio}
    app.ports.audioEnded.send(audioCallback);
    console.log('sound is over');
    console.log(audio);
  } )
})


