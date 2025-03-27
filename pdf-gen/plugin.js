const secondsPerDay = 24 * 60 * 60;
const t = i18next.t;

var app;
var previewed = false;
var iframeWidth = document.getElementById("iframe").style.width;


function preview() {


    if (previewed) {
        document.getElementById("preview").style.display = "none"
        document.getElementById("iframe").style.display = "";
        document.getElementById("panel").style.display = "grid";
        previewed = false;

    } else {
        document.getElementById("preview").src = pdfurl;
        document.getElementById("preview").style.display = ""
        document.getElementById("iframe").style.display = "none"
        document.getElementById("panel").style.display = "none";
        previewed = true;
    }

}


function getLanguage() {
    if (this._lang) {
        return this._lang;
    } else {
        const queryString = window.location.search;
        const urlParams = new URLSearchParams(queryString);
        // this._lang = (urlParams.get('language') ?? navigator.language ?? 'en');
        this._lang = navigator.language;
        console.log("getLanguage() =>", this._lang);
        return this._lang;
    }
}


async function translatePage() {

    const backendOptions = {

        loadPath: '../pdf-gen/locales/{{lng}}/translations.json',
        // don't allow cross domain requests
        crossDomain: false,
        // don't include credentials on cross domain requests
        withCredentials: false,
        // overrideMimeType sets request.overrideMimeType("application/json")
        overrideMimeType: false,
    }
    await i18next.use(i18nextHttpBackend).init({
        lng: getLanguage(),
        debug: false,
        saveMissing: false,
        returnNull: false,
        backend: backendOptions,
    }
    );

    document.getElementById("download").innerHTML = t("pdfDownload");
}

function debounce(func, wait, immediate, context) {
    var result;
    var timeout = null;
    return function () {
        var ctx = context || this, args = arguments;
        var later = function () {
            timeout = null;
            if (!immediate) result = func.apply(ctx, args);
        };
        var callNow = immediate && !timeout;
        // Tant que la fonction est appelée, on reset le timeout.
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
        if (callNow) {
            result = func.apply(ctx, args);
        }
        return result;
    };
}

const generateTimelinePdf = debounce((title, groups, config) => {
    // console.log("VECTOR", generate(groups, config));
    generatePDF(generate(groups, config), title);
    document.getElementById("download").disabled = false
        ;
}, 500, false);
// const generateTimelinePdf = debounce((groups, config) => console.log(generate(groups, config)), 100, false);





async function valuesFor(column) {

    let values = [... new Set(rawtable[column])];
    let meta = metas.find(m => m.colId == column);
    if (meta == -1) {
        console.log("ERROR : can't find column", column)
        return [];
    }

    if (meta.type.startsWith("Ref:")) {
        let table = meta.type.substring(4);
        let refa = await grist.docApi.fetchTable(table);
        let refmetas = await getTypes(table);
        let visibleCol = refmetas.find(rm => rm.id == meta.visibleCol);
        if (visibleCol == undefined) {
            console.log("ERROR : can't find visible column")
            return [];
        }

        return values.map(v => ({ value: v, label: refa[visibleCol.colId][refa.id.indexOf(v)] }));
    } else {

        return values.map(v => ({ value: v, label: v + "" }));
    }

}



let rawtable = [];
let _mappings;
let metas = [];

window.addEventListener('load', async (event) => {
    console.log('La page est complètement chargée');

    let options = undefined;
    let currentRecord;
    let settings;
    let title;

    await translatePage();

    app = Elm.PdfSettings.init(
        {
            node: document.getElementById('config'),
            flags: { language: getLanguage() }
        }
    );

    app.ports.elmToJs.subscribe(async obj => {
        console.log("ELM TO JS", obj);
        if (obj.title) {

            const table = await grist.getTable();
            await table.update({
                id: currentRecord.id,
                fields: { [_mappings.title]: obj.title }
            });
        }

        if (obj.settings) {
            const table = await grist.getTable();
            await table.update({
                id: currentRecord.id,
                fields: { [_mappings.data]: JSON.stringify(obj.settings) }
            });
        }
    });

    app.ports.generate.subscribe(async obj => {

        // console.log("GENERATE", obj);

        let content = settings.content;
        let sorts = settings.sort;

        sorts = sorts.map(s => {
            if (s.direction == "Ascending") {
                return { key: s.field, order: 1 }
            } else if (s.direction == "Descending") {
                return { key: s.field, order: -1 }
            } else {
                return { key: s.field, order: -1 }
            }
        });



        indexes = new Set(rawtable.id.map((v, i) => i));


        const filters = settings.filters ?? {}
        for (var key in filters) {
            let filter = filters[key];
            if (filter.exclude) {
                rawtable[key].forEach((v, i) => { if (filter.values.includes(v)) indexes.delete(i) });
            } else {
                rawtable[key].forEach((v, i) => { if (!filter.values.includes(v)) indexes.delete(i) });
            }

        }


        const refsIdSet = new Set(settings.content);
        if (settings.group) refsIdSet.add(settings.group);
        if (settings.subGroup) refsIdSet.add(settings.subGroup);
        const refsId = [...refsIdSet];

        // console.log("REFSID", refsId);

        const tables = {};

        for (let id of refsId) {
            const meta = metas.find(m => m.colId == id);
            let table = {};
            if (meta && meta.type.startsWith("Ref:")) {
                let tableName = meta.type.substring(4);
                let refa = await grist.docApi.fetchTable(tableName);
                let refmetas = await getTypes(tableName);
                let visibleCol = refmetas.find(rm => rm.id == meta.visibleCol);
                if (visibleCol !== undefined) {
                    table = Object.fromEntries(
                        refa.id.map((id, idx) => [id, refa[visibleCol.colId][idx]])
                    );

                }
            }

            tables[id] = table;
        }


        // console.log("TABLES", tables);


        let groups = [], gidx = {};
        let minDate, maxDate;

        let choicemeta = metas.find(m => m.colId == settings.color)?.widgetOptions;

        if (choicemeta) {

            choicemeta = choicemeta.choiceOptions;

        }

        indexes = [...indexes];


        if (sorts.length === 0) {
            indexes.sort((a, b) => {
                return rawtable.manualSort[a] - rawtable.manualSort[b]
            });
        } else {
            // console.log("SORTS", sorts);
            indexes.sort((a, b) => {
                return sorts.reduce((acc, sort) => {
                    if (acc === 0) {
                        let x = rawtable[sort.key][a], y = rawtable[sort.key][b];

                        if (typeof x === typeof y) {
                            switch (typeof x) {
                                case "number":
                                    return sort.order * (x - y);
                                    break;
                                case "string":
                                    return sort.order * x.localeCompare(y);
                                    break;
                            }
                            return acc;

                        } else {
                            return acc;
                        }
                    } else {
                        return acc;
                    }

                }, 0)
            });

        }

        indexes.forEach(idx => {


            let gid = rawtable[settings.group][idx];
            let glabel = tables[settings.group][gid] || gid;
            let start = rawtable[settings.fromField][idx] * 1000;
            let end = rawtable[settings.toField][idx] * 1000;

            if (settings.subGroup) {
                let sgid = rawtable[settings.subGroup][idx];
                gid = gid + " : " + sgid;
                glabel = [glabel, tables[settings.subGroup][sgid] || sgid];
            }

            let group = gidx[gid] !== undefined ? groups[gidx[gid]] : { label: glabel, sections: [] }
            if (gidx[gid] == undefined) {
                gidx[gid] = groups.length;
                groups.push(group);

            }



            let tlabels = content.map(key => {
                const tid = rawtable[key][idx] || "";
                return tables[key][tid] || tid;
            });

            group.sections.push({
                label: tlabels,
                start: start,
                end: end,
                color: choicemeta ? choicemeta[rawtable[settings.color][idx]]?.fillColor || "grey" : "grey"
            });

            if (minDate === undefined || start < minDate) {
                minDate = start;
            }

            if (maxDate === undefined || end > maxDate) {
                maxDate = end;
            }



        });

        let from = new Date(settings.fromDate ? settings.fromDate : minDate);
        let to = new Date(settings.toDate ? settings.toDate : maxDate);

        // console.log("FROM TO", mappedRecord.fromDate, mappedRecord.toDate);

        // console.log("GROUPS", new Date(from), new Date(to), groups);
        config = {
            title: title
            , fromDate: from
            , toDate: to
            , vpages: settings.vpages
            , hpages: settings.hpages
            , groupsFontSize: settings.groupsFontSize // points
            , tasksFontSize: settings.tasksFontSize // points
            , hoursFontSize: settings.hoursFontSize // points
            , mode: settings.orientation === "Horizontal" ? 0 : 1 // Horizontal, Vertical
            , orientation: settings.layout === "Landscape" ? 0 : 1 // Portrait, Landscape
            , align: settings.align === "Left" ? "left" : (settings.align === "Middle" ? "center" : "right")
            , multiline: settings.multiline
            , papersize: settings.papersize
        }

        // console.log("CONFIG", config);


        // console.log(generate(groups, config));

        // console.log("GROUPS", groups);
        generateTimelinePdf(title, groups, config);
    });


    grist.ready({
        requiredAccess: 'full',
        columns: [
            {
                name: "title", // What field we will read.
                title: t("title"), // Friendly field name.
                optional: false, // Is this an optional field.
                type: "Text", // What type of column we expect.
                //   description: "D", // Description of a field.
                allowMultiple: false, // Allows multiple column assignment.
                strictType: true
            },
            {
                name: "data",
                title: t("settings"),
                optional: false,
                type: "Text",
                allowMultiple: false,
                strictType: true
            }


        ]
        // allowSelectBy: true

    });




    grist.onRecord(async (record, mappings) => {
        // console.log("onRecord", record);

        document.getElementById("iframe").src = "";
        document.getElementById("download").disabled = true;

        const tables = await grist.docApi.fetchTable('_grist_Tables');
        // console.log("TABLES", tables.tableId);


        mappedRecord = grist.mapColumnNames(record, mappings);
        // console.log('MAPPING', mappings);

        currentRecord = record;
        _mappings = mappings;

        title = record[mappings.title];
        settings = safeParse(record[mappings.data]);

        // console.log("SETTINGS", settings);

        metas = [];

        if (settings?.table) {
            rawtable = await grist.docApi.fetchTable(settings.table);
            // console.log("RAWTABLE", rawtable)

            metas = await getTypes(settings.table);


            metas = metas.filter(m => m.colId == "manualSort" || m.colId.startsWith("gristHelper_") ? false : true);
            metas = metas.map(t => {
                t.widgetOptions = safeParse(t.widgetOptions);
                // t.values = [];
                return t;
            });
            // console.log("--- METAS --- ", metas)
        }


        if (settings?.filters) {
            for (const column in (settings.filters)) {
                const values = await valuesFor(column);
                const col = metas.find(c => c.colId == column);
                if (col) {
                    col.values = values;
                }

            }
        }

        // console.log("--- METAS --- ", metas)


        if (app) {

            app.ports.jsToElm.send({
                title: record[mappings.title],
                settings: settings,
                tables: tables.tableId,
                fields: metas,
                language: getLanguage()
            });
        }


    });

    grist.onOptions(opts => {
        if (!options) {
            options = opts ? opts : undefined;
        }
    })


});




async function getTypes(tableId) {
    const tables = await grist.docApi.fetchTable('_grist_Tables');
    const columns = await grist.docApi.fetchTable('_grist_Tables_column');
    const fields = Object.keys(columns);
    const tableRef = tables.id[tables.tableId.indexOf(tableId)];


    const colIndexes = columns.parentId.map((id, i) => [id, i]).filter(item => item[0] === tableRef).map(item => item[1]);


    const types = colIndexes.map(index => {
        return Object.fromEntries(fields.map(f => [f, columns[f][index]]));
    });

    return types;
}


function safeParse(value) {
    try {
        return JSON.parse(value);
    } catch (err) {
        return null;
    }
}
