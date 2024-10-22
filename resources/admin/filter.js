import { addCloseListener, csrfToken, icon, notifyUser, globalErrorMsg, postIUrlencoded } from "./utils.js";

MutationObserver = window.MutationObserver || window.WebKitMutationObserver;
const observerConfig = {
    subtree: true,
    childList: true
}

const errorClass = "error-message";
const notificationId = "hx-notification";

const form = document.getElementById("filter-form");

function isTextInput(ele) {
    let tagName = ele.tagName;
    if (tagName === "INPUT") {
        let validType = ['text', 'number', 'search'];
        let eleType = ele.type;
        return validType.includes(eleType);
    }
    return false;
}

const isListOperator = (operator) => {
    return ["contains_all", "contains_some", "contains_none"].includes(operator)
}

const disableValueInput = (o) => ["empty", "not_empty"].includes(o)

const isQueryKey = (key) => {
    return ["participation", "tag"].includes(key)
}

const findChildPredicates = (wrapper) => {
    return [...wrapper.querySelector(".predicate-wrapper").children].filter((elm) => elm.classList.contains("predicate"))
}

const findValueInputs = (predicate) => {
    return [...predicate.querySelectorAll("[data-name='value[]'],[name='value'],[name='value[]']")];
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
                    case "multi_select":
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
        let value = null;
        let valueDisabled = disableValueInput(operator);
        if (!valueDisabled) {
            let isList = isListOperator(operator)
            if (isList) {
                var values = []
                if (isQueryKey(key)) {
                    values = [...outerPredicate.querySelectorAll(`[data-query="results"] [name="value[]"]:checked`)];
                } else {
                    values = [...outerPredicate.querySelectorAll(`[name="value[]"]:checked`)];
                }
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
        }

        if (!error) {
            let predicate = {
                "key": key,
                "operator": operator
            }
            if (value) {
                predicate.value = value;
            }
            return {
                [predicateType]: predicate
            }
        } else {
            throw "Please fill out all fields";
        }
    } else {
        throw 'Unknown predicate type';
    }
}

function configRequest(e, form) {
    if (isTextInput(e.srcElement) && !e.srcElement.value) {
        e.preventDefault()
    }
    const isPredicateType = e.detail.parameters.predicate;
    const allowEmpty = e.detail.parameters.allow_empty_values;
    const isSubmit = e.target.type === "submit"
    const isSearchForm = e.detail.elt.name === "search";
    const filterId = form.dataset.filter;
    if (filterId) {
        e.detail.parameters.filter = filterId;
    }

    if (isPredicateType || isSubmit || isSearchForm) {
        const elm = isSubmit ? form.querySelector(".predicate") : e.target.closest('.predicate');
        try {
            if (isSearchForm) {
                // Exclude currently selected experiments form query results
                var wrapper = e.detail.elt.closest("[data-query='wrapper']");
                var exclude = [...wrapper.querySelectorAll(`[data-query="results"] [name="value[]"]:checked`)].map(elt => elt.value);
                e.detail.parameters["exclude[]"] = exclude;
            } else {
                e.detail.parameters.query = predicateToJson(elm, allowEmpty);
            }
            // Only relevant when creating a filter template
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
            notifyUser(notificationId, "error", error)
        }
    }
    var event = new Event('submit');
    form.dispatchEvent(event);
}

// Should this update every time, the filter gets adjusted but not saved?
const updateContactCount = async (form) => {
    const target = document.getElementById("contact-counter");
    let message = "-"
    const parseQuery = () => {
        try {
            const predicate = form.querySelector(".predicate");
            return predicateToJson(predicate, false)
        } catch (error) {
            return false
        }
    }

    if (target) {
        const action = target.dataset.action;
        const spinner = icon(["icon-spinner-outline", "rotate"])
        target.innerHTML = "";
        target.appendChild(spinner);
        try {
            const query = parseQuery();
            const body = { query: JSON.stringify(query), _csrf: csrfToken(form) }
            if (query) {
                const response = await postIUrlencoded(action, body)
                const data = await response.json();
                if (!response.ok) {
                    throw (data.message || response.statusText || globalErrorMsg)
                }
                if (response.status < 200 || response.status > 300) {
                    notifyUser(notificationId, "error", data.message)
                } else {
                    message = data.count
                }
            }
        } catch (error) {
            message = globalErrorMsg;
            console.error(error)
        };
        target.innerHTML = message
    }
}

function addRemovePredicateListener(form, wrapper) {
    const el = wrapper || form;
    [...el.querySelectorAll("[data-delete-predicate]")].forEach(elm => {
        elm.addEventListener("click", (e) => {
            e.currentTarget.closest(".predicate").remove();
            updateContactCount(form);
        })
    })
}

function addOperatorChangeListeners(form, wrapper) {
    const el = wrapper || form;
    [...el.querySelectorAll("[name='operator']")].forEach((el) => {
        el.addEventListener("change", (e) => {
            const predicate = e.target.closest('.predicate');
            const inputs = findValueInputs(predicate)
            inputs.forEach(input => input.disabled = disableValueInput(e.currentTarget.value))
            updateContactCount(form);
        })
    })
}

const hideError = (input) => {
    const error = input.closest(".form-group").querySelector(".error-message");
    if (input.value && error) {
        error.innerHTML = "";
    }
}

const addInputChangeListeners = (form, wrapper) => {
    const el = wrapper || form;
    const listener = (e) => {
        hideError(e.currentTarget);
        updateContactCount(form);
    }
    const valueInputs = findValueInputs(el)
    valueInputs.forEach(input => {
        input.addEventListener("input", listener);
    })
}

const addKeyChangeListeners = (form, wrapper) => {
    const el = wrapper || form;
    const listener = (e) => {
        hideError(e.currentTarget);
        updateContactCount(form);
    }
    [...el.querySelectorAll('[name="key"]')].forEach(select => {
        select.addEventListener("input", listener)
    })
}

const addMultiSelectObserver = (form, wrapper) => {
    const el = wrapper || form;
    [...el.querySelectorAll("[data-search-selection]")].forEach(results => {
        const observer = new MutationObserver(function (mutations, observer) {
            updateContactCount(form)
        })
        observer.observe(results, observerConfig);
    })
}

const addEventListeners = (form, htmxElt) => {
    addRemovePredicateListener(form, htmxElt);
    addOperatorChangeListeners(form, htmxElt);
    addInputChangeListeners(form, htmxElt);
    addMultiSelectObserver(form, htmxElt);
    addKeyChangeListeners(form, htmxElt);
}

export function initFilterForm() {
    if (form) {
        const submitButton = document.getElementById("submit-filter-form");
        submitButton.addEventListener('htmx:beforeSwap', (e) => {
            if (e.detail.xhr.status > 200 && e.detail.xhr.status < 300) {
                e.detail.shouldSwap = true;
            }
        })
        addEventListeners(form);
        form.addEventListener('htmx:afterSwap', (e) => {
            addEventListeners(form, e.detail.elt)
            addCloseListener(notificationId);
            updateContactCount(form);
        })
        form.addEventListener('htmx:configRequest', (e) => configRequest(e, form))
    }
}
