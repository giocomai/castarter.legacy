var system = require('system');
var page = require('webpage').create();

var url = system.args[1];
var destination = system.args[2];

page.settings.resourceTimeout = 10000;

setTimeout(function(){
    setInterval(function () {
var fs = require('fs');
var page = require('webpage').create();
page.open(url, function () {
    console.log(page.content);
try {
    fs.write(destination, page.content, 'w');
    } catch(e) {
        console.log(e);
    }
    phantom.exit();
});
    }, 20000);
}, 1);

