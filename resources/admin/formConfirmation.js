function getFormValues(form) {
    var inputs = form.querySelectorAll("[name]");
    return [...inputs].reduce((acc, curr) => {
        return { ...acc, [curr.name]: curr.value }
    }, {})
}

export function initFormConfirmation() {
    var originalValues = {}
    const forms = [...document.querySelectorAll("form[data-detect-unsaved-changes]")];
    function handleWindowUnload(e) {
        var unsubmitted = forms.find((form, i) => {
            var values = getFormValues(form);
            return (JSON.stringify(values) != JSON.stringify(originalValues[i]))
        })
        if (unsubmitted) {
            e.preventDefault();
            e.returnValue = false;
        }
    }

    forms.forEach((form, i) => {
        var values = getFormValues(form);
        originalValues[i] = values;
        form.addEventListener('submit', () => {
            window.removeEventListener("beforeunload", handleWindowUnload);
        })
    })

    window.addEventListener('beforeunload', handleWindowUnload);
}
