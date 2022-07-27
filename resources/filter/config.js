export default config = {
    keys: [
        {
            name: "name",
            type: "string"
        },
        {
            name: "email",
            type: "string"
        },
        {
            name: "verified",
            type: "bool"
        },
        {
            name: "birthday",
            type: "date"
        },
        {
            name: "age",
            type: "number"
        },
        {
            name: "tags",
            type: "list"
        }
    ], operators: [
        {
            name: "less",
            types: ["number", "date"]
        },
        {
            name: "less or equal",
            types: ["number", "date"]
        },
        {
            name: "greater",
            types: ["number", "date"]
        },
        {
            name: "greater or equal",
            types: ["number", "date"]
        },
        {
            name: "equal",
            types: ["number", "date", "string", "bool"]
        },
        {
            name: "not equal",
            types: ["number", "date", "string", "bool"]
        },
        {
            name: "like",
            types: ["string"]
        },
        {
            name: "contains some",
            types: ["list"]
        },
        {
            name: "contains none",
            types: ["list"]
        },
        {
            name: "contains all",
            types: ["list"]
        }
    ], predicateTypes: ["and", "or", "not", "predS", "predM"]
}
