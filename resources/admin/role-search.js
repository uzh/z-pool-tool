import { addCloseListener, addInputListeners, csrfToken, destroySelected, notifyUser } from "./utils.js";

const form = document.getElementById("role-search-form");
const notificationId = "role-search-notification";

function configRequest(e, form) {
    const isSubmit = e.target.type === "submit"
    const isSearchForm = Boolean(e.detail.elt.classList.contains("query-input"));
    e.detail.parameters._csrf = csrfToken(form);

    if (isSubmit || isSearchForm) {
        try {
            var checked_results = [...form.querySelectorAll(`[data-query="results"] [name="value[]"]:checked`)].map(elt => elt.value);
            if (isSearchForm) {
                // Exclude currently selected experiments form query results
                e.detail.parameters["exclude[]"] = checked_results;
                e.detail.parameters.role = form.querySelector("[name='role']").value;
                e.detail.parameters.exclude_roles_of = form.querySelector("[name='exclude_roles_of']").value;
            }
            else if (isSubmit) {
                e.detail.parameters["target[]"] = checked_results;
                e.detail.parameters.role = form.querySelector("[name='role']").value;
            }
        } catch (error) {
            console.error(error);
            e.preventDefault();
            notifyUser(notificationId, "error", error)
        }
    }

    var event = new Event('submit');
    form.dispatchEvent(event);
}

export function initRoleSearchForm() {
    if (form) {
        const submitButton = document.getElementById("submit-role-search-form");
        submitButton.addEventListener('htmx:beforeSwap', (e) => {
            if (e.detail.xhr.status > 200 && e.detail.xhr.status < 300) {
                e.detail.shouldSwap = true;
            }
        });
        // Query event listeners
        [...form.querySelectorAll("[data-query='input']")].forEach(e => addInputListeners(e));
        [...form.querySelectorAll("[data-query='results'] [data-id]")].forEach(e =>
            e.querySelector(".toggle-item").addEventListener("click", () => destroySelected(e))
        );

        form.addEventListener('htmx:afterSwap', (e) => {
            if (e.detail.elt.dataset.query) {
                addInputListeners(e.detail.elt)
            }
            addCloseListener(notificationId);
        })

        form.addEventListener('htmx:configRequest', (e) => configRequest(e, form))
    }
}
