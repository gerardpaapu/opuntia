var parser = require('intl-messageformat-parser');
var printer = require('intl-messageformat-parser/dist/printer');

exports.parseImpl = function (src) {
    return function (just) {
        return function (none) {
            try {
                return just(parser.parse(src));
            } catch (_) {
                return none;
            }
        };
    };
};

exports.printImpl = function (ast) {
    return printer.printAST(ast);
};
