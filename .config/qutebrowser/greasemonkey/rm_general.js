// ==UserScript==
// @name     RM various improvements
// @version  1
// @require      https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js
// @require https://gist.githubusercontent.com/BrockA/2625891/raw/9c97aa67ff9c5d56be34a55ad6c18a314e5eb548/waitForKeyElements.js
// @match    https://rm.baum.ru/*
// ==/UserScript==

const print = (s) => console.log(`GM: ${s}`)

function hideEmptyRedmineFields() {
    document.querySelectorAll('.attributes .attribute').forEach(field => {
        const valueElement = field.querySelector('.value');
        if (valueElement != null) {
            const text = valueElement.textContent.trim();
            if (text === "" || text === "-")
                field.style.display = 'none';
        }
    });
}

if (window.location.pathname.match(/^\/issues\/(\d+)/)) {
    print('matched an issue page');

    function insert_zero_width_space(node) {
        node[0].textContent += '\u200B';
        print('zero-width-space inserted to ' + String(node));
    };

    // Fix a bug in issue-number rendering: the number styled as `inline-block`,
    // so that status would be next to it and not below. Okay per se, but then
    // if you select the issue number, the status will get selected as well in
    // Chome-based browsers (it's not visible, but if you press Copy, you'll get
    // status copied too). This makes sure you get selected only the text you
    // selected.
    waitForKeyElements("div#content > h2", insert_zero_width_space, true);

    // Remove annoying query params that Redmine randomly inserts into the URL
    // while navigating to an issue.
    print('removing query params');
    window.history.replaceState(null, '', location.pathname + location.hash);
}

if (window.location.pathname.match(/^\/issues\/(\d+)/)) {
    print('matched an issue page');

    function my_close_task() {
        const select = document.getElementById('issue_status_id');
        const options = Array.from(select.options);
        const closedOption = options.find(option => option.text === 'Закрыта');
        if (!closedOption) {
            alert('Error: Option "Закрыто" not found');
            return;
        }
        select.value = closedOption.value;

        const dueDateInput = document.querySelector('input[name="issue[due_date]"]');
        const today = new Date().toISOString().split('T')[0];
        dueDateInput.value = today;

        const submitInput = document.querySelector('input[type="submit"][name="commit"][value="Отправить"]');
        submitInput.click();
    }

    const divs = document.querySelectorAll('div#content > div.contextual');
    divs.forEach(div => {
        const button = document.createElement('button');
        button.textContent = 'Close task';
        button.addEventListener('click', my_close_task);
        div.prepend(button);
    });
}

hideEmptyRedmineFields()
