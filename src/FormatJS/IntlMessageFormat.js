var parser = require('intl-messageformat-parser');
var printer = require('intl-messageformat-parser/dist/printer');

exports.type_literal = parser.TYPE.literal;
exports.type_argument = parser.TYPE.argument;
exports.type_number = parser.TYPE.number;
exports.type_date = parser.TYPE.date;
exports.type_time = parser.TYPE.time;
exports.type_select = parser.TYPE.select;
exports.type_plural = parser.TYPE.plural;
exports.type_pound = parser.TYPE.pound;
exports.type_tag = parser.TYPE.tag;

exports.skeleton_type_number = 0;
exports.skeleton_type_dateTime = 1;

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
