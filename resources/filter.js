// TODO: import as separate file only on filter page?
const csrfToken = () => {
    return document.getElementById("filter-form").querySelector('[name="_csrf"]').value;
}

// TODO: let user close notification, maybe display somewhere else
const notifyUser = (classname, msg) => {
    const notificationId = "filter-notification";
    const icon = document.createElement("i");
    icon.classList.add("notification-close", "icon-close-outline");

    const inner = document.createElement("div")
    inner.classList.add("notification", classname);
    inner.innerHTML = msg;
    inner.appendChild(icon);

    const wrapper = document.createElement("div");
    wrapper.classList.add("notification-fixed");
    wrapper.id = notificationId
    wrapper.appendChild(inner);

    const notification = document.getElementById(notificationId)
    notification.parentElement.replaceChild(wrapper, notification)
}

const findChildPredicates = (wrapper) => {
    return [...wrapper.querySelector(".predicate-wrapper").children].filter((elm) => elm.classList.contains("predicate"))
}

const addRequiredError = (elm) => {
    if (elm) {
        const wrapper = elm.closest(".form-group");
        let error = document.createElement("span");
        error.innerHTML = "This field is required."
        error.classList.add("error-message", "help");
        wrapper.appendChild(error);
    }
}

// Should this update every time, the filter gets adjusted but not safed?
const updateContactCount = async () => {
    const target = document.getElementById("contact-counter");
    const id = target.dataset.experimentId;
    try {
        const response = await fetch(`/admin/experiments/${id}/contact-count`);
        const data = await response.json();
        if (!response.ok) {
            throw (data.message || response.statusText || "An Error occurred")
        }
        if (response.status < 200 || response.status > 300) {
            notifyUser("error", data.message)
        } else {
            target.innerHTML = data.count
        }
    } catch (error) {
        notifyUser("error", error)
    };
}

const predicateToJson = (outerPredicate, allowEmpty = false) => {
    const predicateType = outerPredicate.dataset.predicate;
    if (["or", "and"].includes(predicateType)) {
        const andOrPredicates = findChildPredicates(outerPredicate)
        return {
            [predicateType]: [...andOrPredicates].map((p) => predicateToJson(p, allowEmpty))
        }
    } else if (predicateType === "not") {
        const notPredicate = findChildPredicates(outerPredicate)[0];
        return {
            [predicateType]: predicateToJson(notPredicate, allowEmpty)
        }
    } else if (predicateType === "pred") {
        const toValue = (valueInput) => {
            let value = {}
            if (valueInput && valueInput.value) {
                var inputDataType = valueInput.dataset.inputType;
                switch (inputDataType) {
                    case "str":
                        value = {
                            [inputDataType]: valueInput.value
                        }
                        break;
                    case "nr":
                        value = {
                            [inputDataType]: parseFloat(valueInput.value)
                        }
                        break;
                    case "date":
                        value = {
                            [inputDataType]: valueInput.value
                        }
                        break;
                    case "bool":
                        value = {
                            [inputDataType]: (value == "true")
                        }
                        break;
                    case "option":
                        value = {
                            [inputDataType]: valueInput.value
                        }
                        break;
                    default:
                        value = {
                            ["str"]: valueInput.value
                        }
                }
            }
            return value
        }
        var error = false;
        const findElm = (attr) => outerPredicate.querySelector(`[name="${attr}"]`);
        var inputs = ["key", "operator"].map(findElm);
        let [key, operator] = inputs.map((input) => {
            if (!allowEmpty && !(input && input.value)) {
                error = true;
                addRequiredError(input);
            } else {
                return input ? input.value : null
            }
        })
        const valueInput = findElm("value");
        let value = {}
        if (!allowEmpty && !(valueInput && valueInput.value)) {
            error = true;
            addRequiredError(valueInput);
        } else {
            value = toValue(valueInput)
        }
        if (!error) {
            return {
                [predicateType]: {
                    "key": key,
                    "operator": operator,
                    value
                }
            }
        } else {
            notifyUser("error", "Please fill out all fields.")
            throw "Missing values";
        }
    } else {
        throw 'Unknown predicate type';
    }
}

function addRemovePredicateListener(element) {
    [...element.querySelectorAll("[data-delete-predicate]")].forEach(elm => {
        elm.addEventListener("click", (e) => {
            e.currentTarget.closest(".predicate").remove();
        })
    })
}

function configRequest(e, form) {
    const isPredicateType = e.target.name === "predicate";
    const isSubmit = e.target.type === "submit"
    e.detail.parameters._csrf = csrfToken();
    if (isPredicateType || isSubmit) {
        const elm = isSubmit ? form.querySelector(".predicate") : e.target.closest('.predicate');
        try {
            e.detail.parameters.filter = predicateToJson(elm, isPredicateType);
        } catch (error) {
            console.error(error)
            e.preventDefault();
            notifyUser("error", error)
        }
    }
}

export function initFilter() {
    const form = document.getElementById("filter-form");
    if (form) {
        const submitButton = document.getElementById("submit-filter-form");
        submitButton.addEventListener('htmx:beforeSwap', (e) => {
            if (e.detail.xhr.status === 400) {
                e.detail.shouldSwap = true;
            }
        })
        addRemovePredicateListener(form);
        form.addEventListener('htmx:afterSwap', (e) => {
            addRemovePredicateListener(e.detail.elt)
        })
        updateContactCount()
        form.addEventListener('htmx:configRequest', (e) => configRequest(e, form))
    }
}
