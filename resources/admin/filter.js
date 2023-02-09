const errorClass = "error-message";
const globalErrorMsg = "An Error occurred";
const csrfToken = () => {
    return document.getElementById("filter-form").querySelector('[name="_csrf"]').value;
}
const form = document.getElementById("filter-form");

const icon = (classnames) => {
    const i = document.createElement("i");
    i.classList.add(...classnames)
    return i
}

const fadeOut = (elm) => {
    elm.classList.add("fade-out", "no-delay");
}

function addCloseListener() {
    const notification = document.getElementById("filter-notification");
    const iconClose = notification ? notification.querySelector(".notification-close") : false;
    if (iconClose) {
        iconClose.addEventListener("click", () => fadeOut(notification));
    }
}

const notifyUser = (classname, msg) => {
    const notificationId = "filter-notification";
    const inner = document.createElement("div")
    inner.classList.add("notification", classname);
    inner.innerHTML = msg;
    const closeIcon = icon(["icon-close", "notification-close"])
    const wrapper = document.createElement("div");
    wrapper.classList.add("notification-fixed");
    wrapper.id = notificationId
    inner.appendChild(closeIcon);
    wrapper.appendChild(inner);
    const notification = document.getElementById(notificationId)
    notification.parentElement.replaceChild(wrapper, notification)
    addCloseListener();
}

const isListOperator = (operator) => {
    return ["contains_all", "contains_some", "contains_none"].includes(operator)
}

const findChildPredicates = (wrapper) => {
    return [...wrapper.querySelector(".predicate-wrapper").children].filter((elm) => elm.classList.contains("predicate"))
}

const addRequiredError = (elm) => {
    if (elm) {
        const wrapper = elm.closest(".form-group");
        [...wrapper.getElementsByClassName(errorClass)].forEach((elm) => elm.remove());
        let error = document.createElement("span");
        error.innerHTML = "This field is required."
        error.classList.add(errorClass, "help");
        wrapper.appendChild(error);
    }
}

// Should this update every time, the filter gets adjusted but not safed?
const updateContactCount = async () => {
    const target = document.getElementById("contact-counter");
    if (target) {
        const action = target.dataset.action;
        const spinner = icon(["icon-spinner-outline", "rotate"])
        target.appendChild(spinner);
        try {
            const response = await fetch(action);
            const data = await response.json();
            if (!response.ok) {
                throw (data.message || response.statusText || globalErrorMsg)
            }
            if (response.status < 200 || response.status > 300) {
                notifyUser("error", data.message)
            } else {
                target.innerHTML = data.count
            }
        } catch (error) {
            target.innerHTML = globalErrorMsg;
            notifyUser("error", error)
        };
    }
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
    } else if (predicateType === "template") {
        let input = outerPredicate.querySelector('[name="template"]');
        if (!input.value && !allowEmpty) {
            addRequiredError(input)
        } else {
            const value = input.value || "";
            return {
                [predicateType]: value
            }
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
                            [inputDataType]: (valueInput.value == "true")
                        }
                        break;
                    case "option":
                        value = {
                            [inputDataType]: valueInput.value
                        }
                        break;
                    case "language":
                        value = {
                            [inputDataType]: valueInput.value
                        }
                        break;
                    case "list":
                        value = {
                            ["option"]: valueInput.value
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
        let value;
        let isList = isListOperator(operator)
        if (isList) {
            const values = [...outerPredicate.querySelectorAll(`[name="value[]"]:checked`)];
            value = values.map(toValue)
        } else {
            const valueInput = findElm("value");
            if (!allowEmpty && !(valueInput && valueInput.value)) {
                error = true;
                addRequiredError(valueInput);
            } else {
                value = toValue(valueInput)
            }
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
    const isPredicateType = e.detail.parameters.predicate;
    const allowEmpty = e.detail.parameters.allow_empty_values;
    const isSubmit = e.target.type === "submit"
    e.detail.parameters._csrf = csrfToken();
    const filterId = form.dataset.filter;
    if (filterId) {
        e.detail.parameters.filter = filterId;
    }
    if (isPredicateType || isSubmit) {
        const elm = isSubmit ? form.querySelector(".predicate") : e.target.closest('.predicate');
        try {
            e.detail.parameters.query = predicateToJson(elm, allowEmpty);
            const title = document.querySelector('#filter-form [name="title"]');
            if (title && isSubmit) {
                if (!title.value) {
                    throw "Please add a title.";
                } else {
                    e.detail.parameters.title = title.value;
                }
            }
        } catch (error) {
            console.error(error)
            e.preventDefault();
            notifyUser("error", error)
        }
    }
    var event = new Event('submit');
    form.dispatchEvent(event);
}

export function initFilterForm() {
    if (form) {
        const submitButton = document.getElementById("submit-filter-form");
        submitButton.addEventListener('htmx:beforeSwap', (e) => {
            if (e.detail.xhr.status > 200 && e.detail.xhr.status < 300) {
                e.detail.shouldSwap = true;
            }
        })
        addRemovePredicateListener(form);
        form.addEventListener('htmx:afterSwap', (e) => {
            addRemovePredicateListener(e.detail.elt)
            if (e.detail.target.type === "submit") {
                updateContactCount();
            }
            addCloseListener();
        })
        updateContactCount()
        form.addEventListener('htmx:configRequest', (e) => configRequest(e, form))
    }
}
