// TODO: import as separate file only on filter page?

const csrfToken = () => {
    return document.getElementById("filter-form").querySelector('[name="_csrf"]').value;
}

// TODO: let user close notification, maybe display somewhere else
const notifyUser = (classname, msg) => {
    const icon = document.createElement("i");
    icon.classList.add("notification-close", "icon-close-outline");

    const inner = document.createElement("div")
    inner.classList.add("notification", classname);
    inner.innerHTML = msg;
    inner.appendChild(icon);

    const wrapper = document.createElement("div");
    wrapper.classList.add("notification-fixed");
    wrapper.appendChild(inner);
    document.querySelector("body").appendChild(wrapper);
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

const buildFormBody = (data) => {
    var formBody = [];
    for (var property in data) {
        var encodedKey = encodeURIComponent(property);
        var encodedValue = encodeURIComponent(data[property]);
        formBody.push(encodedKey + "=" + encodedValue);
    }
    return formBody.join("&");
}

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
        var error = false;
        const findElm = (attr) => outerPredicate.querySelector(`[name="${attr}"]`);
        var inputs = ["key", "operator"].map(findElm);
        let [key, operator] = inputs.map((input) => {
            if (!allowEmpty && !(input && input.value)) {
                error = true;
                addRequiredError(input);
            }
            return input ? input.value : input
        })
        let value;
        const valueInput = findElm("value");
        if (valueInput && valueInput.value) {
            var inputDataType = valueInput.dataset.inputType;
            var inputValue;
            switch (inputDataType) {
                case "str":
                    inputValue = value;
                    break;
                case "nr":
                    inputValue = parseFloat(value);
                    break;
                case "date":
                    inputValue = value;
                    break;
                case "bool":
                    inputValue = (value == "true");
                    break;
                case "option":
                    inputValue = value;
                    break;
                default:
                    inputDataType = "str";
                    inputValue = value;
            }
            value = {
                [inputDataType]: inputValue
            }
        }
        if (!allowEmpty && !value) {
            error = true;
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

function addBeforeRequestListener(predicate) {
    predicateSelects = predicate.querySelectorAll('[name="predicate"]');
    [...predicateSelects].forEach(elm => {
        elm.addEventListener('htmx:configRequest', (e) => {
            const predicate = elm.closest('.predicate');
            try {
                e.detail.parameters.filter = predicateToJson(predicate, true);
                e.detail.parameters._csrf = csrfToken();
            } catch (error) {
                console.error(error)
                e.preventDefault();
                notifyUser("error", error)
            }
        })
    })
    triggers = predicate.querySelectorAll('[name="key"], [data-new-predicate] button');
    [...triggers].forEach(elm => {
        elm.addEventListener('htmx:configRequest', (e) => {
            e.detail.parameters._csrf = csrfToken();
        })
    })
}

function addAfterSwapListener(predicate) {
    elms = predicate.querySelectorAll('.predicate, [data-new-predicate]');
    [...elms].forEach(elm => {
        elm.addEventListener('htmx:afterSwap', (e) => {
            addHtmxListeners(e.detail.elt)
        })
    })
}
function addRemovePredicateListener(predicate) {
    [...predicate.querySelectorAll("[data-delete-predicate]")].forEach(elm => {
        elm.addEventListener("click", (e) => {
            e.currentTarget.closest(".predicate").remove();
        })
    })
}

function addHtmxListeners(predicate) {
    addBeforeRequestListener(predicate);
    addAfterSwapListener(predicate);
    addRemovePredicateListener(predicate);
}

export function initFilter() {
    const submitBtn = document.getElementById("submit-filter-form");
    if (submitBtn) {
        submitBtn.addEventListener("click", (e) => {
            const form = document.getElementById("filter-form");
            e.preventDefault();
            const predicate = form.querySelector(".predicate");
            let json;
            try {
                json = predicateToJson(predicate);
            } catch (e) {
                console.error(e)
            }
            if (json) {
                const url = form.dataset.action;
                const body = buildFormBody({ filter: JSON.stringify(json) })
                fetch(url, {
                    method: "POST",
                    headers: {
                        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
                    },
                    body
                })
                    .then((response) => response.json())
                    .then((data) => {
                        if (data.success) {
                            notifyUser("success", data.message)
                        } else {
                            notifyUser("error", data.message)
                        }
                    });
            }
        })
    }
    const form = document.getElementById("filter-form");
    if (form) {
        addHtmxListeners(form);
        updateContactCount()
    }
}
