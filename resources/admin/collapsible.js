
const activeClass = "active"

const animationDuration = 300;

const closeCollapsible = (el) => {
    if (el.classList.contains(activeClass)) {
        const body = el.querySelector(".collapsible-body");
        body.style.height = 0;
        el.classList.remove(activeClass)
        setTimeout(() => {
            body.style.removeProperty('height');
        }, animationDuration)
    }
}

export const initCollapsible = () => {
    const lists = document.querySelectorAll(".collapsible-list")
    lists.forEach(wrapper => {
        const collapsibles = wrapper.querySelectorAll(".collapsible");
        collapsibles.forEach(el => {
            el.querySelector(".collapsible-header").addEventListener("click", () => {
                const isActive = el.classList.contains(activeClass)
                collapsibles.forEach(el => closeCollapsible(el))
                if (!isActive) {

                    const offsetHeight = el.querySelector(".collapsible-content").offsetHeight;
                    const body = el.querySelector(".collapsible-body");
                    body.style.height = `${offsetHeight}px`;
                    el.classList.add(activeClass)
                    setTimeout(() => {
                        body.style.removeProperty('height');
                    }, animationDuration)
                }
            })
        })
    })
}
