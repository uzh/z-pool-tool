export const csrfToken = (form) => {
    return form.querySelector('[name="_csrf"]').value;
}

export const icon = (classnames) => {
    const i = document.createElement("i");
    i.classList.add(...classnames)
    return i
}

export const fadeOut = (elm) => {
    elm.classList.add("fade-out", "no-delay");
}

export function addCloseListener(notificationId) {
    const notification = document.getElementById(notificationId);
    const iconClose = notification ? notification.querySelector(".notification-close") : false;
    if (iconClose) {
        iconClose.addEventListener("click", () => fadeOut(notification));
    }
}

export const notifyUser = (notificationId, classname, msg) => {
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
    addCloseListener(notificationId);
}

export function destroySelected(item) {
    item.remove();
}

export function addInputListeners(queryInput) {
    var wrapper = queryInput.closest("[data-query='wrapper']");
    var results = wrapper.querySelector("[data-query='results']");

    [...queryInput.querySelectorAll("[data-id]")].forEach(item => {
        item.addEventListener("click", () => {
            results.appendChild(item);
            item.querySelector(".toggle-item").addEventListener("click", () => destroySelected(item));
        }, { once: true })
    })
}
