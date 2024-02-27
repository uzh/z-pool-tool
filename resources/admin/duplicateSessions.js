const removeSubformHandler = (e) => {
    const group = e.currentTarget.closest(["[data-duplicate-form]"]);
    if (parseInt(group.dataset.duplicateForm)) {
        group.remove()
    }
}

const addRemoveSubformListeners = (container) => {
    [...container.querySelectorAll("[data-remove-group]")].forEach(el => {
        el.removeEventListener("click", removeSubformHandler)
        el.addEventListener("click", removeSubformHandler)
    })
}

const duplicateSessionFormEvents = (form, subformWrapper) => {
    if (form && subformWrapper) {
        form.addEventListener('htmx:configRequest', (evt) => {
            const counter = [...subformWrapper.querySelectorAll("[data-duplicate-form]")].reduce((acc, el) => {
                const id = el.dataset.duplicateForm;
                return id && id > acc ? id : acc
            }, 0)
            evt.detail.parameters['counter'] = counter;
        });

        addRemoveSubformListeners(subformWrapper)
        form.addEventListener('htmx:afterSettle', () => {
            addRemoveSubformListeners(subformWrapper)
        })
    }
}

export const initDuplicateSessionForm = () => {
    const form = document.getElementById("session-duplication-form")
    const subformWrapper = document.getElementById("session-duplication-subforms")

    if (form && subformWrapper) {
        duplicateSessionFormEvents(form, subformWrapper)
    }
}
