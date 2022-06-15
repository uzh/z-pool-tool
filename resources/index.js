import 'htmx.org'
import flatpickr from 'flatpickr'
import { German } from "flatpickr/dist/l10n/de.js"
import Default from "flatpickr/dist/l10n/default.js"

import 'flatpickr/dist/themes/light.css'
import './index.css'

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
        enableTime: true,
        altInput: true,
        time_24hr: true,
        minuteIncrement: 1,
        allowInput: true
    }
}

function initDatepicker() {
    flatpickr.localize({ firstDayOfWeek: 1 })
    document.querySelectorAll('.datepicker').forEach(e => {
        var f = flatpickr(e, {
            ...globalConfig(e),
            altFormat: "d.m.Y H:i",
            dateFormat: "Z",
            onChange: function (selectedDates, dateStr, instance) {
                const helpText = e.closest(".form-group").querySelector(".datepicker-msg")
                if (new Date() > selectedDates[0]) {
                    const message = e.dataset.warnPast || "The selected date is in the past."
                    helpText.textContent = message;
                } else {
                    helpText.textContent = "";
                }
            },
        })
        f._input.onkeydown = () => false
    });
    document.querySelectorAll('.spanpicker').forEach(e => {
        var f = flatpickr(e, {
            ...globalConfig(e),
            noCalendar: true,
            altFormat: "H:i",
            dateFormat: "H:i:S"
        })
        f._input.onkeydown = () => false
    });
}

initDatepicker();
