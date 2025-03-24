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

const isDate = date => date && date.getMonth() ? true : false

const largestDate = (dates) => {
    const filtered = dates.filter((date) => isDate(date))
    return filtered.length ? filtered.reduce(((a, b) => a > b ? a : b), 0) : null;
}

export function initDatepicker(container = document) {
    flatpickr.localize({ firstDayOfWeek: 1 })
    const renderDatepickers = (container) => {
        [...container.getElementsByClassName(datePickerClass)].forEach(e => {
            const { disablePast, disableFuture, disableTime, minDate: dataMinDate, minInputElement } = e.dataset;
            const classlist = Array.from(e.classList);
            const dateFormat = disableTime ? "Y-m-d" : "Z";
            const altFormat = disableTime ? "d.m.Y" : "d.m.Y H:i";
            const formGroup = e.closest(".form-group");
            const minInputEl = minInputElement && document.querySelector(`[name="${minInputElement}"]`);
            const minInputElValue = minInputEl && minInputEl.value
            const minInputElementDate = minInputElValue && new Date(minInputElValue)

            const parsedMinDate = dataMinDate && new Date(dataMinDate);
            const nowDate = disablePast && new Date()

            const minDate = largestDate([parsedMinDate, minInputElementDate, nowDate])


            const f = flatpickr(e, {
                ...globalConfig(e),
                altFormat,
                dateFormat,
                enableTime: !disableTime,
                minDate,
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
                onReady: function (value, originalValue, instance) {
                    const { altInput, hourElement, minuteElement } = instance;
                    altInput.disabled = e.readOnly; // Workaround for https://github.com/flatpickr/flatpickr/issues/2492
                    classlist.forEach(cls => altInput.classList.add(cls))

                    // Manually set time when using keyboard
                    hourElement.addEventListener("keyup", (e) => {
                        const date = instance.selectedDates[0] || new Date();
                        date.setHours(e.currentTarget.value)
                        instance.setDate(date);
                    })

                    minuteElement.addEventListener("keyup", (e) => {
                        const date = instance.selectedDates[0] || new Date();
                        date.setMinutes(e.currentTarget.value)
                        instance.setDate(date);
                    })
                },
            })
            if (minInputEl) {
                minInputEl.addEventListener("change", (e) => {
                    const value = new Date(e.currentTarget.value)
                    const minDate = largestDate([value, parsedMinDate, nowDate]);
                    f.set('minDate', minDate)
                })
            }
        });
    }
    renderDatepickers(container)

    document.addEventListener('htmx:afterSettle', (e) => {
        renderDatepickers(e.detail.elt)
    })
}
