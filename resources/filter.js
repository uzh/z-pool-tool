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
    const wrapper = elm.closest(".form-group");
    let error = document.createElement("span");
    error.innerHTML = "This field is required."
    error.classList.add("error-message", "help");
    wrapper.appendChild(error);
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
        var success = true;
        var inputs = ["key", "operator", "value"].map((attr) => outerPredicate.querySelector(`[name="${attr}"]`))
        let [key, operator, value] = inputs.map((input) => {
            if (!input.value && !allowEmpty) {
                success = false;
                addRequiredError(input);
            }
            return input.value
        })
        if (success) {
            var inputDataType = inputs[2].dataset.inputType;
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
            return {
                [predicateType]: {
                    "key": key,
                    "operator": operator,
                    value: {
                        [inputDataType]: inputValue
                    }
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
    elms = predicate.querySelectorAll('[name="predicate"]');
    [...elms].forEach(elm => {
        elm.addEventListener('htmx:configRequest', (e) => {
            const predicate = elm.closest('.predicate');
            try {
                e.detail.parameters.filter = predicateToJson(predicate, true);
            } catch (error) {
                e.preventDefault();
                notifyUser("error", error)
            }
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

function addHtmxListeners(predicate) {
    addBeforeRequestListener(predicate);
    addAfterSwapListener(predicate);
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
        [...document.querySelectorAll("[data-delete-predicate]")].forEach(elm => {
            elm.addEventListener("click", (e) => {
                e.currentTarget.closest(".predicate").remove();
            })
        })
    }
}
