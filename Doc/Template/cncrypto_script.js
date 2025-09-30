const units = [
<!--#UNIT_LIST#-->
];

function initUnitList() {
    const unitList = document.getElementById('unitList');
    units.forEach(unit => {
        const li = document.createElement('li');
        li.className = 'unit-item';
        li.textContent = unit;
        li.onclick = () => loadUnit(unit);
        unitList.appendChild(li);
    });
}

function loadUnit(unitName) {
    const frame = document.getElementById('contentFrame');
    frame.src = `${unitName}.html`;
    
    document.querySelectorAll('.unit-item').forEach(item => {
        item.classList.remove('active');
        if (item.textContent === unitName) {
            item.classList.add('active');
        }
    });
}

function initSearch() {
    const searchInput = document.getElementById('searchInput');
    searchInput.addEventListener('input', function() {
        const searchTerm = this.value.toLowerCase();
        const unitItems = document.querySelectorAll('.unit-item');
        
        unitItems.forEach(item => {
            const unitName = item.textContent.toLowerCase();
            if (unitName.includes(searchTerm)) {
                item.style.display = 'block';
            } else {
                item.style.display = 'none';
            }
        });
    });
}

document.addEventListener('DOMContentLoaded', function() {
    initUnitList();
    initSearch();
});