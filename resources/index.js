import 'htmx.org'
import './index.css'
import { initDatepicker } from "./flatpickr.js"
import { initSortable } from "./sortable.js"
initDatepicker();
initSortable();

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

const wrapString = (str) => `["${str}"]`

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
const filterForm = document.getElementById("submit-filter-form");
if (filterForm) {
    filterForm.addEventListener("click", (e) => {
        const form = document.getElementById("filter-form");
        e.preventDefault();
        const buildQuery = (outerPredicate) => {
            const predicateType = outerPredicate.querySelector('select[name="predicate"]').value;
            if (["or", "and"].includes(predicateType)) {
                const andOrPredicates = findChildPredicates(outerPredicate)
                return `(${wrapString(predicateType)},(${[...andOrPredicates].map((p) => buildQuery(p)).join(",")}))`
            } else if (predicateType === "not") {
                const notPredicate = findChildPredicates(outerPredicate)[0];
                return `(${wrapString(predicateType)},${buildQuery(notPredicate)})`
            } else if (predicateType === "pred_s") {
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
                    var inputElm = inputs[2];
                    inputTypeAttr = inputElm.getAttribute("type");
                    var inputDataType;
                    var inputValue;
                    if (inputElm.classList.contains("flatpickr-input")) {
                        inputDataType = "date"
                        inputValue = `"${value}"`;
                    } else {
                        switch (inputTypeAttr) {
                            case "text":
                                inputDataType = "str";
                                inputValue = `"${value}"`;
                                break;
                            case "number":
                                inputDataType = "nr";
                                inputValue = value;
                                break;
                            case "date":
                                inputDataType = "date";
                                inputValue = `"${value}"`;
                                break;
                            case "checkbox":
                                inputDataType = "bool";
                                inputValue = value;
                                break;
                            default:
                                inputDataType = "str";
                                inputValue = `"${value}"`;
                        }
                    }
                    return `(${wrapString(predicateType)},[${[wrapString(key), wrapString(operator), `{"${inputDataType}":${inputValue}}`].join(",")}])`
                } else {
                    notifyUser("error", "Please fill out all fields.")
                    throw "Missing values";
                }
            } else {
                throw 'Unknown predicate type';
            }
        }
        const predicate = form.querySelector(".predicate");
        let json;
        try {
            json = buildQuery(predicate);
        } catch (e) {
            console.error(e)
        }
        if (json) {
            const url = form.dataset.action;
            const body = buildFormBody({ filter: json })
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
