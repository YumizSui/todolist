!function($) {
    $(document).ready(() => {
        show_message_list();
    });
}(jQuery);

function reset() {
    const $ = jQuery;
    $("#app").empty();
}

function show_registration_form() {
    reset();
    const $ = jQuery;
    $("#app");
}

function update_message_list() {
    fetch("/formapp/api/messages", {method: "GET"})
        .then(response => { response.json(); })
        .then(json => { return show_message_list(json); });
}

function show_message_list(json) {
    reset();
    const root = create_default_dom();
    $("#app").append(root);
    if(json.length == 0) {
        $("#app").append("<span>登録されたメッセージはありません</span>");
    } else {
        const table = $("<table>");
        table.html("<tr><th></th><th>投稿者</th><th>性別</th><th>投稿日時</th></tr>");
        table.addClass("w3-table w3-border w3-border");
        for(var entry of json) {
            const row = $("<tr>");
            const column = $("<td>");
            const idColumn = column.clone();
            idColumn.html(entry['id']);
            row.append(idColumn);
            const nameColumn = column.clone();
            nameColumn.html(entry['name']);
            row.append(nameColumn);
            const genderColunm = column.clone();
            genderColumn.html(entry['gender']);
            row.append(genderColumn);
            const timeColumn = column.clone();
            timeColumn.html(entry['createdAt']);
            row.append(timeColumn);
            table.append(row);
        }
        $("#app").append(table);
    }
    return false;
}

function show_nolist() {
    reset();
    const $ = jQuery;
}

function create_default_dom() {
    const $ = jQuery;
    const root = $("<div>");
    root.addClass("w3-row");
    const labelField = $("<span>");
    labelField.addClass("w3-half");
    labelField.html("<h3>登録済みメッセージ一覧</h3>");
    root.append(labelField);
    const buttonField = $("<span>");
    buttonField.addClass("w3-right");
    buttonField.html('<a href="#" onclick="" class="w3-button w3-green">新規登録</a>');
    root.append(buttonField);
    return root;
}

function show_message() {
    reset();
}