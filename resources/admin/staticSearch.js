const createTag = (option) => {
    const item = document.createElement("span");
    item.setAttribute("data-selection-item", "");
    item.innerHTML = option.innerHTML;
    const icon = document.createElement("i");
    icon.classList.add("icon-close");
    const input = document.createElement("input");
    input.setAttribute("type", "checkbox");
    input.setAttribute("data-input-type", "option")
    input.checked = true;
    input.setAttribute("hidden", "")
    input.value = option.dataset.value;
    input.name = "value[]";
    item.appendChild(icon);
    item.appendChild(input);
    return item;
}

const addRemoveItemListener = (item) => {
    item.querySelector(".icon-close").addEventListener("click", () => {
        item.remove()
    }, { once: true })
}

export const initStaticSearch = (target) => {
    const container = target || document;
    container.querySelectorAll('[data-search="static"]').forEach((el) => {
        const wrapper = el.closest(".form-group");
        const results = wrapper.querySelector("[data-search-selection]");
        const optionsWrapper = wrapper.querySelector(".data-list");
        const options = optionsWrapper.querySelectorAll(".data-item")
        const filterOptions = () => {
            const value = String(el.value).toLocaleLowerCase();
            const selected = [...results.querySelectorAll("[data-selection-item] input")].map((item) => item.value)
            options.forEach((option) => {
                if (option.innerText.includes(value) && !selected.includes(option.dataset.value)) {
                    option.style.display = "flex";
                } else {
                    option.style.display = "none";
                }
            })
        }
        el.addEventListener("focusin", () => {
            optionsWrapper.classList.add("active");
        })
        document.addEventListener("click", (e) => {
            if (!container.contains(e.target)) {
                optionsWrapper.classList.remove("active");
            }
        });
        el.addEventListener("keyup", () => {
            filterOptions();
        });

        options.forEach((option) => {
            option.addEventListener("click", () => {
                const item = createTag(option);
                option.style.display = "none";
                results.appendChild(item);
                addRemoveItemListener(item);
                filterOptions();
            })
        });

        [...results.querySelectorAll("[data-selection-item]")].forEach((item) => addRemoveItemListener(item));
        filterOptions();
    })
}
