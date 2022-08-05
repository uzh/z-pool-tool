import { html, LitElement } from 'lit';
import { AndOrPredicate, ListPredicate, Predicate, SinglePredicate } from './predicate.js';

// This is just a help
const mockup = {
    0: {
        type: "and",
        predicates: {
            0: {
                type: "single",
                predicates: {
                    0: {},
                    1: {}
                }
            },
            1: {
                type: "single",
                predicates: {}
            }
        }
    }
}

function updateFilterObject(parentIds, id, data, filter) {
    if (parentIds.length == 0) {
        let oldPredicate = {}
        if(filter[id] && filter[id].predicateType === data.predicateType) {
            oldPredicate = filter[id]
        }
        return {
            ...filter,
            [id]: {
                // should children be reset or kept?
                // probably depending on the change? and to or => keep / and to single => delete
                // predicates: filter[id].predicates,
                ...oldPredicate,
                ...data
            }
        }
    } else {
        return {
            ...filter,
            [parentIds[0]]: {
                ...filter[parentIds[0]],
                predicates: updateFilterObject(parentIds.slice(1), id, data, filter[parentIds[0]].predicates || {})
            }
        }
    }
}

function wrapString (str) {
    return `["${str}"]`
}

function formatToYojson(json) {
    return Object.entries(json).map(([key, value]) => {
        if(["and", "or"].includes(value.predicateType)) {
            return `${wrapString(value.predicateType)}`
        } else {
            return `${wrapString(value.predicateType)}`
        }
      })
}

export class ContactFilter extends LitElement {
    static get properties() {
        return {
        };
    }

    constructor() {
        super();
        this.filterValue = {};
        this.action = String(this.getAttribute('data-action'));
        this.csrf = String(this.getAttribute('data-csrf'));
    }

    submit(e) {
        e.preventDefault();
        const body = formatToYojson(this.filterValue);
        console.log(body);
        // How to pass the csrf token??
        fetch(this.action, {
            method: "POST",
            headers: {
                'Content-Type': 'application/json',
                'X-CSRF-TOKEN': this.csrf,
            },
            body: JSON.stringify(this.filterValue)
        }).then(res => {
            console.log("Request complete! response:", res);
        });
    }

    setFilterValue(parentIds, id, data) {
        this.filterValue = updateFilterObject(parentIds, id, data, this.filterValue);
    }

    createRenderRoot() {
        return this;
    }

    render() {
        let predicate = new Predicate(0, [], (parentIds, id, data) => this.setFilterValue(parentIds, id, data))
        return html`
            <div>
                ${predicate}
                <button type="submit" class="success gap" @click=${(e) => this.submit(e)}>
                    Find contacts
                </button>
            </div>
            `;
    }
}

window.customElements.define('contact-filter-single-predicate', SinglePredicate);
window.customElements.define('contact-filter-and-or-predicate', AndOrPredicate);
window.customElements.define('contact-filter-list-predicate', ListPredicate);
window.customElements.define('contact-filter-predicate', Predicate);
window.customElements.define('contact-filter', ContactFilter);
