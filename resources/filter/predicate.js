import { html, LitElement } from 'lit';
import config from "./config.js"

const { keys, operators, predicateTypes } = config;

const singleKeys = keys.filter(k => k.type != "list");
const listKeys = keys.filter(k => k.type === "list");

class Base extends LitElement {
    constructor(id, parentIds, setFilterValue) {
        super();
        this.id = id;
        this.parentIds = parentIds;
        this.setFilterValue = setFilterValue;
    }

    createRenderRoot() {
        return this;
    }
}

export class ListPredicate extends Base {
    static get properties() {
        return {
            key: {
                type: {
                    name: String,
                    type: String,
                }
            },
            operator: {
                type: {
                    name: String,
                    types: Array,
                }
            },
            predicate: { type: Object },
        };
    }

    constructor(id, parentIds, setFilterValue) {
        super(id, parentIds, setFilterValue);
        this.key = null;
        this.operator = null;
        this.predicate = new Predicate(0, [...this.parentIds, id], this.setFilterValue);
    }

    setKey(e) {
        this.key = keys.find((k) => {
            return k.name === e.currentTarget.value;
        })
    }

    setOperator(e) {
        this.operator = operators.find((o) => {
            return o.name === e.currentTarget.value;
        })
    }

    setValue(e) {
        this.values = e.currentTarget.value;
    }

    render() {
        return html`
        <div class="flexcolumn stack">
            <div class="switcher flex-gap">
                <div class="form-group">
                    <label>Key</label>
                    <div class="select">
                        <select @change=${(e) => this.setKey(e)}>
                        ${!this.key ? html`<option selected disabled>Please select</option>` : null}
                        ${listKeys.map(key => {
            return html`<option value="${key.name}" ${this.key?.name === key.name ? "selected" : null}>${key.name}</option>`
        })}
                        </select>
                    </div>
                </div>
                <div class="form-group">
                    <label>Operator</label>
                    ${this.key ? html`<div class="select">
                        <select @change=${(e) => this.setOperator(e)}>
                            ${!this.operator ? html`<option selected disabled>Please select</option>` : null}
                            ${operators.map(operator => {
            return (operator.types.includes(this.key.type) ?
                html`<option value="${operator.name}" ${this.operator?.name === operator.name ? "selected" : null}>${operator.name}</option>`
                : null)
        })}
                        </select>
                    </div>` : null}
                </div>
            </div>
            ${this.predicate}
        </div>
    `}
}

export class SinglePredicate extends Base {
    static get properties() {
        return {
            key: {
                type: {
                    name: String,
                    type: String,
                }
            },
            operator: {
                type: {
                    name: String,
                    types: Array,
                }
            },
            value: { type: String },
        };
    }

    constructor(id, parentIds, setFilterValue) {
        super(id, parentIds, setFilterValue);
        this.key = null;
        this.operator = null;
        this.value = null;
    }

    updateValues() {
        const data = {
            key: this.key,
            operator: this.operator,
            value: this.value
        }
        this.setFilterValue(this.parentIds, this.id, data)
    }

    setKey(e) {
        this.key = keys.find((k) => {
            return k.name === e.currentTarget.value;
        })
        this.updateValues()
    }

    setOperator(e) {
        this.operator = operators.find((o) => {
            return o.name === e.currentTarget.value;
        })
        this.updateValues()
    }

    setValue(e) {
        this.value = e.currentTarget.value;
        this.updateValues()
    }

    render() {
        let input = null;
        if (this.operator) {
            let inputType = null;
            switch (this.key?.type) {
                case "string":
                    inputType = "text"
                    break;
                case "number":
                    inputType = "number"
                    break;
                case "date":
                    inputType = "date"
                    break;
                case "book":
                    inputType = "checkbox"
                    break;
                default:
                    inputType = "text"
            }
            input = html`<input @keydown=${(e) => this.setValue(e)} type="${inputType}" value=${this.value}></input>`
        }

        return html`
        <div class="switcher flex-gap">
            <div class="form-group">
                <label>Key</label>
                <div class="select">
                    <select @change=${(e) => this.setKey(e)}>
                    ${!this.key ? html`<option selected disabled>Please select</option>` : null}
                    ${singleKeys.map(key => {
            return html`<option value="${key.name}" ${this.key?.name === key.name ? "selected" : null}>${key.name}</option>`
        })}
                    </select>
                </div>
            </div>
            <div class="form-group">
                <label>Operator</label>
                ${this.key ? html`<div class="select">
                    <select @change=${(e) => this.setOperator(e)}>
                        ${!this.operator ? html`<option selected disabled>Please select</option>` : null}
                        ${operators.map(operator => {
            return (operator.types.includes(this.key.type) ?
                html`<option value="${operator.name}" ${this.operator?.name === operator.name ? "selected" : null}>${operator.name}</option>`
                : null)
        })}
                    </select>
                </div>` : null}
            </div>

            <div class="form-group">
                <label>Value</label>
                ${input}
            </div>
        </div>
    `}
}

export class AndOrPredicate extends Base {
    static get properties() {
        return {
            id: { type: Number },
            predicates: { type: Array }
        };
    }

    constructor(id, parentIds, setFilterValue) {
        super(id, parentIds, setFilterValue);
        this.currentIndex = 0;
        this.predicates = [
            new Predicate(this.currentIndex, [...this.parentIds, this.id], this.setFilterValue),
            new Predicate(this.currentIndex + 1, [...this.parentIds, this.id], this.setFilterValue)]
        this.currentIndex = this.predicates.length
    }

    addPredicate() {
        this.predicates = [...this.predicates, new Predicate(this.currentIndex, [...this.parentIds, this.id], this.setFilterValue)]
        this.currentIndex = this.currentIndex + 1;
    }

    render() {
        return html`
            <div class="flexcolumn stack inset left">
                ${this.predicates.map(p => html`<div>${p}</div>`)}
                <div><button @click=${() => this.addPredicate()}>Add predicate</button></div>
            </div>`
    }
}

export class Predicate extends Base {
    static get properties() {
        return {
            type: { type: String },
            conditions: { type: Array }
        };
    }

    constructor(id, parentIds, setFilterValue) {
        super(id, parentIds, setFilterValue);
        this.type = null;
        this.conditions = []
    }

    setPredicateType(e) {
        const value = e.currentTarget.value
        this.type = predicateTypes.includes(value) ? value : null;
        this.setFilterValue(this.parentIds, this.id, { predicateType: this.type })
    }

    render() {
        let predicate;
        switch (this.type) {
            case "and":
            case "or":
                predicate = new AndOrPredicate(this.id, this.parentIds, this.setFilterValue);
                break;
            case "not":
            case "predS":
                predicate = new SinglePredicate(this.id, this.parentIds, this.setFilterValue)
                break;
            case "predM":
                predicate = new ListPredicate(this.id, this.parentIds, this.setFilterValue)
                break;
            default:
                null;
        }

        return html`
        <div class="flexcolumn stack border inset-sm | predicate" data-id="${this.id}" data-parent-id="${this.parentIds.join("-")}">
            <div class="form-group">
                <label>Type of predicate</label>
                <div class="select">
                    <select @change=${(e) => this.setPredicateType(e)}>
                    ${!this.type ? html`<option selected disabled>Please select</option>` : null}
                    ${predicateTypes.map(predicate => {
            return html`<option value="${predicate}" ${this.predicate === predicate ? "selected" : null}>${predicate}</option>`
        })}
                    </select>
                </div>
            </div>
            ${predicate ? html`<div>${predicate}</div>` : null}
        </div>
    `}
}

