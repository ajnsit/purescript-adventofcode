// Readline lets us tap into the process events
const readline = require("readline");

// Allows us to listen for events from stdin
const ev = readline.emitKeypressEvents(process.stdin);

let keystrokeBuffer = [];
let needingKeys = [];

// Raw mode gets rid of standard keypress events and other
// functionality Node.js adds by default

const onKeyPress = (str, key) => {
  if (needingKeys.length) {
    needingKeys.pop()(key);
  } else {
    keystrokeBuffer.push(key);
  }

  // "Raw" mode so we must do our own kill switch
  if (key.sequence === "\u0003") {
    process.exit();
  }
};

exports.setNodeRawModeImpl = function(error, success) {
  if (process.stdin.isTTY) {
    process.stdin.setRawMode(true);

    // Start the keypress listener for the process
    process.stdin.on("keypress", onKeyPress);
    success();
  } else {
    error("Terminal is not TTY");
  }

  return function(a, b, c) {
    c();
  };
};

exports.exitProcessImpl = function(error, success) {  if (keystrokeBuffer.length) {
    success(keystrokeBuffer.shift());
  } else {
    needingKeys.push(success);
  }

  process.stdin.setRawMode(false);
  process.stdin.removeAllListeners("keypress");
  process.exit();
  success();

  return function(a, b, c) {
    c();
  };
};

exports.getNextKeySyncImpl = function() {
  if (keystrokeBuffer.length) {
      return keystrokeBuffer.shift();
  } else {
      return null;
  }
}

exports.getNextKeyImpl = function(error, success) {
  if (keystrokeBuffer.length) {
    success(keystrokeBuffer.shift());
  } else {
    needingKeys.push(success);
  }

  return function(a, b, cancelSuccess) {
    needingKeys = needingKeys.filter(function(x) {
      return x !== success;
    });
    cancelSuccess();
  };
};
