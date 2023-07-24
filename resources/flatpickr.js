import flatpickr from 'flatpickr'
import { German } from "flatpickr/dist/l10n/de.js"
import Default from "flatpickr/dist/l10n/default.js"

import 'flatpickr/dist/themes/light.css';
const datePickerClass = "datepicker"

function getLocale(elm) {
    const locale = elm.dataset.language;
    if (locale) {
        switch (locale) {
            case "DE":
                return {
                    ...German
                };
            default:
                return {
                    ...Default
                }
        }
    } else {
        return {
            ...Default
        }
    }
}

function globalConfig(e) {
    return {
        locale: {
            ...getLocale(e),
            firstDayOfWeek: 1
        },
        altInput: true,
        time_24hr: true,
        minuteIncrement: 1,
        allowInput: true
    }
}

export function initDatepicker(container = document) {
    flatpickr.localize({ firstDayOfWeek: 1 })
    const renderDatepickers = (container) => {
        [...container.getElementsByClassName(datePickerClass)].forEach(e => {
            var { disablePast, disableFuture, disableTime } = e.dataset;
            var classlist = Array.from(e.classList);
            var dateFormat = disableTime ? "Y-m-d" : "Z";
            var altFormat = disableTime ? "d.m.Y" : "d.m.Y H:i";
            const formGroup = e.closest(".form-group");
            var f = flatpickr(e, {
                ...globalConfig(e),
                altFormat,
                dateFormat,
                enableTime: !disableTime,
                minDate: disablePast ? new Date() : null,
                maxDate: disableFuture ? new Date() : null,
                onChange: function (selectedDates, dateStr, instance) {
                    const helpText = formGroup.querySelector(".datepicker-msg")
                    if (e.dataset.warnPast) {
                        if (new Date() > selectedDates[0]) {
                            helpText.textContent = e.dataset.warnPast;
                        } else {
                            helpText.textContent = "";
                        }
                    }
                },
                onReady: function (value, originalValue, { altInput }) {
                    classlist.forEach(cls => altInput.classList.add(cls))
                }
            })
            f._input.onkeydown = () => false
        });
    }
    renderDatepickers(container)

    document.addEventListener('htmx:afterSettle', (e) => {
        renderDatepickers(e.detail.elt)
    })
}
