'use strict';

import { Elm } from './Main.elm';

require("./styles.scss");

Elm.Main.init({
  node: document.getElementById('root')
});