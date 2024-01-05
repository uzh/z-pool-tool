import { csrfToken } from "./admin/utils.js";
import { initSearch } from "./search.js";
import { initNotification } from '../node_modules/@econ/frontend-framework/dist/main'

const configRequest = (e, form) => {
    if (e.detail.verb.toLowerCase() != "get") {
        e.detail.parameters._csrf = csrfToken(form);
    }
}

export const initHTMX = () => {
    document.addEventListener('htmx:configRequest', (e) => configRequest(e))
    document.addEventListener('htmx:afterSwap', (e) => {
        initSearch(e.detail.elt)
        initNotification()
        // TODO: Check if js_add_modal_close_listener can be called here generally, maybe integrate to framework
    })
}
