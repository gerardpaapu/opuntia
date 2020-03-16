var parser = require('intl-messageformat-parser');
var printer = require('intl-messageformat-parser/dist/printer');

exports.parseImpl = function (src) {
    return function (right) {
        return function (left) {
            try {
                return right(parser.parse(src));
            } catch (e) {
                return left(e);
            }
        };
    };
};

exports.printImpl = function (ast) {
    return printer.printAST(ast);
};

exports.jsonStringify = function (obj) {
  return JSON.stringify(obj, Object.keys(obj).sort(), 2);
};
