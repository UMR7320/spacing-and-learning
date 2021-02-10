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

app.ports.playAudio.subscribe((audio) => {
  var sound = new Howl({
    src: [audio]
  });

  sound.play();
})


