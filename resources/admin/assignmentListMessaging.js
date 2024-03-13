const localStorageKey = "pool-assignment-messaging"
const startSelectBtn = document.querySelector('[data-direct-message="select"]');
const cancelSelectBtn = document.querySelector('[data-direct-message="cancel"]');
const submitBtn = document.querySelector('[data-direct-message="submit"]');
const assignmentsTable = document.getElementById("assignments-table");

// TODO: connect data to session id

let isActive = false;
let checkboxesBuilt = false;
let values = {}

const assignmentRows = () => {
    return [...assignmentsTable.querySelectorAll("tr[data-id]")]
}

const buildCheckbox = (id) => {
    const input = document.createElement("input");
    input.setAttribute("type", "checkbox");
    input.setAttribute("value", id)
    input.dataset.directMessageSelect = "";
    input.checked = Boolean(values[id])
    return input
}

const handleChangeCheckbox = (e) => {
    const el = e.currentTarget;
    id = el.value;
    if (el.checked) {
        values[id] = true;
    } else {
        delete value.id
    }
}

const prependCheckboxes = () => {
    if (!isActive || checkboxesBuilt) {
        return
    }
    const rows = assignmentRows();
    rows.forEach(row => {
        const id = row.dataset.id;
        const cell = row.firstChild;
        const checkbox = buildCheckbox(id);
        checkbox.addEventListener("change", handleChangeCheckbox)
        cell.insertBefore(checkbox, cell.firstChild);
    })
    checkboxesBuilt = true;
}

const storeValuesInLocalstorage = () => {
    localStorage.setItem(localStorageKey, JSON.stringify({
        isActive,
        values
    }))
}

const afterSettleListener = (e) => {
    checkboxesBuilt = false;
    prependCheckboxes()
}

const readValuesFromLocalstorage = () => {
    const stored = JSON.parse(localStorage.getItem(localStorageKey));
    values = (stored && stored.values) ? stored.values : {}
    isActive = stored ? stored.isActive : false
}

toggleButtonClassnames = () => {
    if (isActive) {
        startSelectBtn.classList.add("hidden");
        cancelSelectBtn.classList.remove("hidden");
        submitBtn.classList.remove("hidden");
    } else {
        startSelectBtn.classList.remove("hidden");
        cancelSelectBtn.classList.add("hidden");
        submitBtn.classList.add("hidden");
    }
}

const handleCancelClick = () => {
    [...assignmentsTable.querySelectorAll("[data-direct-message-select]")].forEach(el => el.remove());
    assignmentsTable.removeEventListener("htmx:afterSettle", afterSettleListener);
    isActive = false;
    checkboxesBuilt = false;
    toggleButtonClassnames();
}

const handleSubmitClick = (e) => {
    e.detail.parameters['assignment[]'] = Object.keys(values);
}

export const initAssignmentListMessaging = () => {
    window.addEventListener("beforeunload", () => storeValuesInLocalstorage());
    readValuesFromLocalstorage();

    startSelectBtn.addEventListener("click", () => {
        isActive = true;
        prependCheckboxes();
        toggleButtonClassnames();
    })

    cancelSelectBtn.addEventListener("click", handleCancelClick)
    submitBtn.addEventListener('htmx:configRequest', handleSubmitClick)
    prependCheckboxes();
    toggleButtonClassnames();
    assignmentsTable.addEventListener("htmx:afterSettle", afterSettleListener)
}

