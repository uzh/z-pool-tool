const disableSubmit = (elm) => {
    const icon = document.createElement("i")
    icon.classList.add("icon-spinner-outline", "rotate")
    elm.classList.add("has-icon")
    elm.appendChild(icon)
    elm.disabled = true
}

export function initFormSubmitInterferer() {
    const forms = [...document.querySelectorAll("form:not([data-disable-interfere-submit])")];
    forms.forEach(form => {
        let submitted = false
        const submits = form.querySelectorAll('[type="submit"]');
        form.addEventListener("submit", (e) => {
            if (submitted) {
                e.preventDefault()
            }
            submits.forEach(disableSubmit)
            submitted = true
        })
    })
}
