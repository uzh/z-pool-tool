
const dropdownWidth = 200;

export const initButtonList = () => {
    const els = document.querySelectorAll(".button-list");

    els.forEach(el => {
        const dropdown = el.querySelector(".dropdown")
        if (!dropdown) {
            return
        }

        var rect = el.getBoundingClientRect();
        const windowWidth = window.innerWidth || document.documentElement.clientWidth;
        if (rect.right + dropdownWidth >= windowWidth) {
            el.classList.add("left")
        }
    })
}
