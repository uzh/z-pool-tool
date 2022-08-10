import 'htmx.org'
import './index.css'
import { initDatepicker } from "./flatpickr.js"
initDatepicker();
initSortable();


const multiPredicates = ["or", "and"]

// function watchChanges(elements) {
//     elements.forEach(elm => {
//         const currentValue = elm.value;
//         elm.addEventListener('htmx:beforeRequest', (e) => {
//             e.detail.target.addEventListener('htmx:afterSwap', (e) => {
//                 console.log(e.detail.target)
//             });
//             if (multiPredicates.includes(currentValue) && multiPredicates.includes(e.detail.elt.value)) {
//                 e.preventDefault();
//             }
//         });

//     })
// }

// window.addEventListener('DOMContentLoaded', () => {
//     watchChanges(document.querySelectorAll('select[name="predicate"]'));
// });

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

document.getElementById("submit-filter-form").addEventListener("click", (e) => {
    const form = document.getElementById("filter-form");
    e.preventDefault();
    const buildQuery = (outerPredicate) => {
        const predicateType = outerPredicate.querySelector('select[name="predicate"]').value;
        if (multiPredicates.includes(predicateType)) {
            const andOrPredicates = findChildPredicates(outerPredicate)
            return `(${wrapString(predicateType)},(${[...andOrPredicates].map((p) => buildQuery(p)).join(",")}))`
        } else if (predicateType === "not") {
            const notPredicate = findChildPredicates(outerPredicate)[0];
            return `(${wrapString(predicateType)},${buildQuery(notPredicate)})`
        } else if (predicateType === "pred_s") {
            var success = true;
            var inputs = ["key", "operator", "value"].map((attr) => outerPredicate.querySelector(`[name="${attr}"]`))
            var inputTypeAttr = inputs[2].getAttribute("type");
            var inputDataType;
            switch (inputTypeAttr) {
                case "text":
                    inputDataType = "str";
                    break;
                case "number":
                    inputDataType = "nr";
                    break;
                case "date":
                    inputDataType = "date";
                    break;
                case "checkbox":
                    inputDataType = "bool";
                    break;
                default:
                    inputDataType = "str";
            }
            let [key, operator, value] = inputs.map((input) => {
                if (!input.value) {
                    success = false;
                    addRequiredError(input);
                }
                return input.value
            })
            if (success) {
                // TODO: Use correct data type
                return `(${wrapString(predicateType)},[${[wrapString(key), wrapString(operator), `{"str":"${value}"}`].join(",")}])`
                // return `(${wrapString(predicateType)},[${[wrapString(key), wrapString(operator), `{"${inputDataType}":"${value}"}`].join(",")}])`
            } else {
                throw "Missing values";
            }
        } else {
            throw 'Unknown predicate type';
        }
    }
    const predicate = form.querySelector(".predicate");
    let json;
    json = buildQuery(predicate);
    // try {
    //     json = buildQuery(predicate);
    // } catch (e) {
    //     console.error(e)
    // }
    if (json) {
        const url = form.dataset.action;
        const body = buildFormBody({ filter: json })
        fetch(url, {
            method: "POST",
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
            },
            body
        }).then((response) => response.json())
            .then((data) => console.log(data));
    }
})
