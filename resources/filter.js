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

const predicateToJson = (outerPredicate) => {
    const predicateType = outerPredicate.querySelector('select[name="predicate"]').value;
    if (["or", "and"].includes(predicateType)) {
        const andOrPredicates = findChildPredicates(outerPredicate)
        return {
            [predicateType]: [...andOrPredicates].map((p) => predicateToJson(p))
        }
    } else if (predicateType === "not") {
        const notPredicate = findChildPredicates(outerPredicate)[0];
        return {
            [predicateType]: predicateToJson(notPredicate)
        }
    } else if (predicateType === "pred") {
        var success = true;
        var inputs = ["key", "operator", "value"].map((attr) => outerPredicate.querySelector(`[name="${attr}"]`))
        let [key, operator, value] = inputs.map((input) => {
            if (!input.value) {
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

export function initFilter() {
    const filterForm = document.getElementById("submit-filter-form");
    if (filterForm) {
        filterForm.addEventListener("click", (e) => {
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
}
