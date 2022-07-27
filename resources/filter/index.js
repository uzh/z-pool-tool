import { LitElement } from 'lit';
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
        return {
            ...filter,
            [id]: {
                // should children be reset or kept?
                // probably depending on the change? and to or => keep. and to single => delete
                // predicates: filter[id].predicates,
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

export class ContactFilter extends LitElement {
    static get properties() {
        return {
        };
    }

    constructor() {
        super();
        this.filterValue = {};
    }

    setFilterValue(parentIds, id, data) {
        this.filterValue = updateFilterObject(parentIds, id, data, this.filterValue);
        console.log(this.filterValue);
    }

    createRenderRoot() {
        return this;
    }

    render() {
        let predicate = new Predicate(0, [], (parentIds, id, data) => this.setFilterValue(parentIds, id, data))
        return predicate;
    }
}

window.customElements.define('contact-filter-single-predicate', SinglePredicate);
window.customElements.define('contact-filter-and-or-predicate', AndOrPredicate);
window.customElements.define('contact-filter-list-predicate', ListPredicate);
window.customElements.define('contact-filter-predicate', Predicate);
window.customElements.define('contact-filter', ContactFilter);
