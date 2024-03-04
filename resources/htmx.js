import { csrfToken } from "./admin/utils.js";
import { initSearch } from "./search.js";
import { initNotification } from '../node_modules/@econ/frontend-framework/dist/main'
import { initRichTextEditor } from "./admin/richTextEditor.js";

const configRequest = (e, form) => {
    if (e.detail.verb.toLowerCase() != "get") {
        e.detail.parameters._csrf = csrfToken(form);
    }
}

export const initHTMX = () => {
    document.addEventListener('htmx:configRequest', (e) => configRequest(e))
    document.addEventListener('htmx:afterSettle', (e) => {
        initSearch(e.detail.elt)
        initRichTextEditor(e.detail.elt)
        initNotification()

        const modalClose = [...e.detail.elt.querySelectorAll(".modal-close")];
        modalClose.forEach(btn => {
            const modal = btn.closest(".modal");
            if (modal) {
                btn.addEventListener("click", () => {
                    modal.classList.remove("active");
                    modal.setAttribute("aria-hidden", "true");
                })
            }
        })
    })
}
