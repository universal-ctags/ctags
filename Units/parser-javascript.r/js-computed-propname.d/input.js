//
// "node --experimental-modules" accepts this input.
//

// Computed property names (ES2015)
var prop = 'foo';
var x = 'c';
var y = ['d', 'e'];
var o = {
    ['a' + y [0]]: {},
    ['a']: {},			// Tagging
    ['a' + 'b']: {},		// Not tagging
    [x]: {},			// Not tagging
    ['a' + x]: {},        	// Not tagging
    
};

class Model {
}

class User extends Model {
  static get tableName() {
    return "users"
  }

  static get ["json-schema"]() {
    return {
      type: "object",
      optional: [
        "passwordHash",
        "passwordResetRequestedAt",
        "passwordResetToken",
        "verificationToken",
        "verifiedAt",
      ]
    }
  }
}
