/********************************************************************\
 * gnc-option-gtk-ui.cpp -- Gtk Widgets for manipulating options    *
  * Copyright 2022 John Ralls <jralls@ceridwen.us>                  *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <gnc-option.hpp>
#include <gnc-option-impl.hpp>
#include "gnc-option-gtk-ui.hpp"
#include <config.h>  // for scanf format string
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iomanip>
#include <locale>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include <qof.h>
#include <gnc-engine.h> // for GNC_MOD_GUI
#include <gnc-commodity.h> // for GNC_COMMODITY
#include "gnc-account-sel.h" // for GNC_ACCOUNT_SEL
#include "gnc-currency-edit.h" //for GNC_CURRENCY_EDIT
#include "gnc-commodity-edit.h" //for gnc_commodity_get_string
#include "gnc-date-edit.h" // for gnc_date_edit
#include "gnc-date-format.h" //for GNC_DATE_FORMAT
#include "gnc-general-select.h" // for GNC_GENERAL_SELECT
#include "gnc-option-uitype.hpp"
#include "gnc-tree-view-account.h" // for GNC_TREE_VIEW_ACCOUNT
#include "gnc-tree-model-budget.h" // for gnc_tree_model_budget
#include "misc-gnome-utils.h" // for xxxgtk_textview_set_text

/*Something somewhere in windows.h defines ABSOLUTE to something and
 *that contaminates using it in RelativeDateType.  Undef it.
 */
#ifdef ABSOLUTE
#undef ABSOLUTE
#endif

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

//Init the class static.
std::vector<WidgetCreateFunc> GncOptionUIFactory::s_registry{static_cast<size_t>(GncOptionUIType::MAX_VALUE)};
bool GncOptionUIFactory::s_initialized{false};
static void gnc_options_ui_factory_initialize (void);

void
GncOptionUIFactory::set_func(GncOptionUIType type, WidgetCreateFunc func)
{
    s_registry[static_cast<size_t>(type)] = func;
}

void
GncOptionUIFactory::create(GncOption& option, GtkGrid* page, int row)
{
    if (!s_initialized)
    {
        gnc_options_ui_factory_initialize();
        s_initialized = true;
    }
    auto type{option.get_ui_type()};
    auto func{s_registry[static_cast<size_t>(type)]};
    if (func)
        func(option, page, row);
    else
        PERR("No function registered for type %d", static_cast<int>(type));
}

GncOptionGtkUIItem::GncOptionGtkUIItem(GtkWidget* widget,
                                       GncOptionUIType type) :
    GncOptionUIItem{type},
    m_widget{static_cast<GtkWidget*>(g_object_ref(widget))} {}

GncOptionGtkUIItem::GncOptionGtkUIItem(const GncOptionGtkUIItem& item) :
    GncOptionUIItem{item.get_ui_type()},
    m_widget{static_cast<GtkWidget*>(g_object_ref(item.get_widget()))} {}

GncOptionGtkUIItem::~GncOptionGtkUIItem()
{
    if (m_widget)
        g_object_unref(m_widget);
}

void
GncOptionGtkUIItem::set_selectable(bool selectable) const noexcept
{
    if (m_widget)
        gtk_widget_set_sensitive (m_widget, selectable);
}

void
GncOptionGtkUIItem::clear_ui_item()
{
    if (m_widget)
        g_object_unref(m_widget);
    m_widget = nullptr;
}

void
GncOptionGtkUIItem::set_widget(GtkWidget* widget)
{
    if (get_ui_type() == GncOptionUIType::INTERNAL)
    {
        std::string error{"INTERNAL option, setting the UI item forbidden."};
        throw std::logic_error(error);
    }

    if (m_widget)
        g_object_unref(m_widget);

    m_widget = static_cast<GtkWidget*>(g_object_ref(widget));
}

SCM
GncOptionGtkUIItem::get_widget_scm_value(const GncOption& option) const
{
    return SCM_BOOL_F;
}
/* ****************************************************************/
/* Option Widgets                                      */
/* ***************************************************************/

static inline GtkWidget* const
option_get_gtk_widget(const GncOption* option)
{
    if (!option) return nullptr;
    auto ui_item{dynamic_cast<const GncOptionGtkUIItem*>(option->get_ui_item())};
    if (ui_item)
        return ui_item->get_widget();

    return nullptr;
}

static inline void
wrap_check_button (const GncOption& option, GtkWidget* widget, GtkGrid* page_box, int row)
{
    auto enclosing{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
    set_tool_tip(option, enclosing);
    gtk_widget_show_all(enclosing);
    /* attach the option widget to the second column of the grid */
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);
}

class GncGtkBooleanUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkBooleanUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::BOOLEAN} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_TOGGLE_BUTTON(get_widget())};
        gtk_toggle_button_set_active(widget, option.get_value<bool>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_TOGGLE_BUTTON(get_widget())};
        option.set_value(static_cast<bool>(gtk_toggle_button_get_active(widget)));
    }
    SCM get_widget_scm_value(const GncOption& option) const override
    {
        auto widget{GTK_TOGGLE_BUTTON(get_widget())};
        return gtk_toggle_button_get_active(widget) ?
               SCM_BOOL_T : SCM_BOOL_F;
    }
};

template <> void
create_option_widget<GncOptionUIType::BOOLEAN> (GncOption& option,
                                                GtkGrid* page_box, int row)

{
    char *local_name{nullptr};
    auto name{option.get_name().c_str()};
    if (name && *name)
        local_name = _(name);
    auto widget{gtk_check_button_new_with_label (local_name)};

    auto ui_item{std::make_unique<GncGtkBooleanUIItem>(widget)};

    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "toggled",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);

    wrap_check_button(option, widget, page_box, row);
}

class GncGtkStringUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkStringUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::STRING} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_ENTRY(get_widget())};
        gtk_entry_set_text(widget, option.get_value<std::string>().c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_ENTRY(get_widget())};
        option.set_value(std::string{gtk_entry_get_text(widget)});
    }
};

template<> void
create_option_widget<GncOptionUIType::STRING> (GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto enclosing{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_widget_set_hexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    auto widget = gtk_entry_new();
    if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL)
        gtk_entry_set_alignment (GTK_ENTRY(widget), 1.0);
    auto ui_item{std::make_unique<GncGtkStringUIItem>(widget)};

    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    gtk_box_pack_start(GTK_BOX(enclosing), widget, TRUE, TRUE, 0);
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    gtk_widget_show_all(enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);
}

class GncGtkTextUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkTextUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::TEXT} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_TEXT_VIEW(get_widget())};
        xxxgtk_textview_set_text(widget, option.get_value<std::string>().c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_TEXT_VIEW(get_widget())};
        auto str{xxxgtk_textview_get_text(widget)};
        option.set_value(std::string{str});
        g_free (str);
    }
};

template<> void
create_option_widget<GncOptionUIType::TEXT> (GncOption& option, GtkGrid *page_box, int row)
{
    auto scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_NEVER,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_set_border_width(GTK_CONTAINER(scroll), 2);

    auto frame = gtk_frame_new(NULL);
    gtk_container_add(GTK_CONTAINER(frame), scroll);

    auto enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_widget_set_vexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    auto widget = gtk_text_view_new();
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(widget), GTK_WRAP_WORD);
    gtk_widget_set_size_request(widget, 400, -1);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(widget), TRUE);
    gtk_text_view_set_accepts_tab (GTK_TEXT_VIEW(widget), FALSE);

    auto ui_item{std::make_unique<GncGtkTextUIItem>(widget)};
    auto text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(text_buffer), "changed",
                     G_CALLBACK(gnc_option_changed_option_cb), &option);
    gtk_container_add (GTK_CONTAINER (scroll), widget);
    gtk_box_pack_start(GTK_BOX(enclosing), frame, TRUE, TRUE, 0);
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    gtk_widget_show_all(enclosing);
    grid_attach_widget(GTK_GRID(page_box), enclosing, row);
}

class GncGtkCurrencyUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkCurrencyUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::CURRENCY} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_CURRENCY_EDIT(get_widget())};
        auto currency{option.get_value<gnc_commodity*>()};

        if (currency)
            gnc_currency_edit_set_currency(widget, GNC_COMMODITY(currency));
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_CURRENCY_EDIT(get_widget())};
        auto currency = gnc_currency_edit_get_currency(widget);
        option.set_value<gnc_commodity*>(currency);
    }
};

template<> void
create_option_widget<GncOptionUIType::CURRENCY> (GncOption& option, GtkGrid *page_box,
                                                 int row)
{
    auto widget{gnc_currency_edit_new()};
    auto ui_item{std::make_unique<GncGtkCurrencyUIItem>(widget)};
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}

class GncGtkCommodityUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkCommodityUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::COMMODITY} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_GENERAL_SELECT(get_widget())};
        auto commodity{option.get_value<gnc_commodity*>()};

        if (commodity)
            gnc_general_select_set_selected(widget, GNC_COMMODITY(commodity));
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_GENERAL_SELECT(get_widget())};
        auto commodity{gnc_general_select_get_selected(widget)};
        option.set_value<gnc_commodity*>(GNC_COMMODITY(commodity));
    }
};

template<> void
create_option_widget<GncOptionUIType::COMMODITY> (GncOption& option, GtkGrid *page_box,
                                                  int row)
{
    auto widget = gnc_general_select_new(GNC_GENERAL_SELECT_TYPE_SELECT,
                                         gnc_commodity_edit_get_string,
                                         gnc_commodity_edit_new_select,
                                         NULL);

    auto ui_item{std::make_unique<GncGtkCommodityUIItem>(widget)};
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT(GNC_GENERAL_SELECT(widget)->entry), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}

static GtkWidget*
create_multichoice_widget(GncOption& option)
{
    auto num_values = option.num_permissible_values();

    g_return_val_if_fail(num_values >= 0, NULL);
    auto renderer = gtk_cell_renderer_text_new();
    auto store = gtk_list_store_new(1, G_TYPE_STRING);
    /* Add values to the list store, entry and tooltip */
    for (decltype(num_values) i = 0; i < num_values; i++)
    {
        GtkTreeIter iter;
        auto itemstring = option.permissible_value_name(i);
        gtk_list_store_append (store, &iter);
        gtk_list_store_set(store, &iter, 0,
                           (itemstring && *itemstring) ? _(itemstring) : "", -1);
    }
    /* Create the new Combo with tooltip and add the store */
    auto widget{GTK_WIDGET(gtk_combo_box_new_with_model(GTK_TREE_MODEL(store)))};
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT(widget),
                                   renderer, "text", 0);
    g_object_unref(store);

    return widget;
}

class GncGtkMultichoiceUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkMultichoiceUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::MULTICHOICE} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_COMBO_BOX(get_widget())};
        gtk_combo_box_set_active(widget, option.get_value<uint16_t>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_COMBO_BOX(get_widget())};
        option.set_value<uint16_t>(static_cast<uint16_t>(gtk_combo_box_get_active(widget)));
    }
    SCM get_widget_scm_value(const GncOption& option) const override
    {
        auto widget{GTK_COMBO_BOX(get_widget())};
        auto id{gtk_combo_box_get_active(widget)};
        auto value{option.permissible_value(id)};
        return scm_from_utf8_symbol(value);
    }
};

template<> void
create_option_widget<GncOptionUIType::MULTICHOICE> (GncOption& option, GtkGrid *page_box,
                                                    int row)
{
    auto widget{create_multichoice_widget(option)};
    auto ui_item{std::make_unique<GncGtkMultichoiceUIItem>(widget)};
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}


class GncDateEntry
{
public:
    GncDateEntry() = default;
    virtual ~GncDateEntry() = default;
    virtual void set_entry_from_option(GncOption& option) = 0;
    virtual void set_option_from_entry(GncOption& option) = 0;
    // Get the widget that has data
    virtual GtkWidget* get_entry() = 0;
    // Get the widget that gets put on the page
    virtual GtkWidget* get_widget() = 0;
    virtual void toggle_relative(bool) {} //BothDateEntry only
    virtual void block_signals(bool) = 0;
};


using GncDateEntryPtr = std::unique_ptr<GncDateEntry>;

class AbsoluteDateEntry : public GncDateEntry
{
public:
    AbsoluteDateEntry(GncOption& option);
    ~AbsoluteDateEntry() = default;
    void set_entry_from_option(GncOption& option) override;
    void set_option_from_entry(GncOption& option) override;
    GtkWidget* get_entry() override { return GTK_WIDGET(m_entry); }
    GtkWidget* get_widget() override { return GTK_WIDGET(m_entry); }
    void block_signals(bool) override;
private:
    GNCDateEdit* m_entry;
    unsigned long m_handler_id;
};

AbsoluteDateEntry::AbsoluteDateEntry(GncOption& option) :
    m_entry{GNC_DATE_EDIT(gnc_date_edit_new(time(NULL), FALSE, FALSE))}
{
    auto entry = GNC_DATE_EDIT(m_entry)->date_entry;
    m_handler_id = g_signal_connect(G_OBJECT(entry), "changed",
                                    G_CALLBACK(gnc_option_changed_option_cb),
                                    &option);
}

void
AbsoluteDateEntry::block_signals(bool block)
{
    auto entry{G_OBJECT(GNC_DATE_EDIT(m_entry)->date_entry)};
    if (block)
        g_signal_handler_block(entry, m_handler_id);
    else
        g_signal_handler_unblock(entry, m_handler_id);
}

void
AbsoluteDateEntry::set_entry_from_option(GncOption& option)
{
    gnc_date_edit_set_time(m_entry, option.get_value<time64>());
}

void
AbsoluteDateEntry::set_option_from_entry(GncOption& option)
{
    option.set_value<time64>(gnc_date_edit_get_date(m_entry));
}

class RelativeDateEntry : public GncDateEntry
{
public:
    RelativeDateEntry(GncOption& option);
    ~RelativeDateEntry() = default;
    void set_entry_from_option(GncOption& option) override;
    void set_option_from_entry(GncOption& option) override;
    GtkWidget* get_widget() override { return m_entry; }
    GtkWidget* get_entry() override { return m_entry; }
    void block_signals(bool) override;
private:
    GtkWidget* m_entry;
    unsigned long m_handler_id;
};


RelativeDateEntry::RelativeDateEntry(GncOption& option)
{

    auto renderer = gtk_cell_renderer_text_new();
    auto store = gtk_list_store_new(1, G_TYPE_STRING);
    /* Add values to the list store, entry and tooltip */
    auto num = option.num_permissible_values();
    for (decltype(num) index = 0; index < num; ++index)
    {
        GtkTreeIter  iter;
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0,
                            _(option.permissible_value_name(index)), -1);
    }

    /* Create the new Combo with tooltip and add the store */
    m_entry = GTK_WIDGET(gtk_combo_box_new_with_model(GTK_TREE_MODEL(store)));
    gtk_combo_box_set_active(GTK_COMBO_BOX(m_entry), 0);
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(m_entry), renderer, TRUE);
    gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT(m_entry),
                                   renderer, "text", 0);

    g_object_unref(store);

    m_handler_id = g_signal_connect(G_OBJECT(m_entry), "changed",
                                    G_CALLBACK(gnc_option_changed_widget_cb),
                                    &option);
}

void
RelativeDateEntry::set_entry_from_option(GncOption& option)
{
    gtk_combo_box_set_active(GTK_COMBO_BOX(m_entry), option.get_value<uint16_t>());
}

void
RelativeDateEntry::set_option_from_entry(GncOption& option)
{
    option.set_value<uint16_t>(gtk_combo_box_get_active(GTK_COMBO_BOX(m_entry)));
}

void
RelativeDateEntry::block_signals(bool block)
{
    if (block)
        g_signal_handler_block(m_entry, m_handler_id);
    else
        g_signal_handler_unblock(m_entry, m_handler_id);
}

using AbsoluteDateEntryPtr = std::unique_ptr<AbsoluteDateEntry>;
using RelativeDateEntryPtr = std::unique_ptr<RelativeDateEntry>;

class BothDateEntry : public GncDateEntry
{
public:
    BothDateEntry(GncOption& option);
    ~BothDateEntry() = default; //The GncOptionGtkUIItem owns the widget
    void set_entry_from_option(GncOption& option) override;
    void set_option_from_entry(GncOption& option) override;
    GtkWidget* get_widget() override { return m_widget; }
    GtkWidget* get_entry() override;
    void toggle_relative(bool use_absolute) override;
    void block_signals(bool) override;
private:
    GtkWidget* m_widget;
    GtkWidget* m_abs_button;
    AbsoluteDateEntryPtr m_abs_entry;
    GtkWidget* m_rel_button;
    RelativeDateEntryPtr m_rel_entry;
    bool m_use_absolute = true;
    unsigned long m_abs_hdlr;
    unsigned long m_rel_hdlr;
};

static void date_set_absolute_cb(GtkWidget *widget, gpointer data1);
static void date_set_relative_cb(GtkWidget *widget, gpointer data1);

BothDateEntry::BothDateEntry(GncOption& option) :
    m_widget{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)},
    m_abs_button{gtk_radio_button_new(NULL)},
    m_abs_entry{std::make_unique<AbsoluteDateEntry>(option)},
    m_rel_button{
        gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(m_abs_button))},
    m_rel_entry{std::make_unique<RelativeDateEntry>(option)}
{
    gtk_box_set_homogeneous (GTK_BOX(m_widget), FALSE);
    m_abs_hdlr = g_signal_connect(G_OBJECT(m_abs_button), "toggled",
                                  G_CALLBACK(date_set_absolute_cb), &option);
    m_rel_hdlr = g_signal_connect(G_OBJECT(m_rel_button), "toggled",
                                  G_CALLBACK(date_set_relative_cb), &option);

    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_abs_button, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_abs_entry->get_entry(), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_rel_button, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_rel_entry->get_entry(), FALSE, FALSE, 0);

}

GtkWidget*
BothDateEntry::get_entry()
{
    return m_use_absolute ? m_abs_entry->get_entry() : m_rel_entry->get_entry();
}

void
BothDateEntry::toggle_relative(bool use_absolute)
{
    m_use_absolute = use_absolute;

    gtk_widget_set_sensitive(GTK_WIDGET(m_abs_entry->get_widget()),
                             m_use_absolute);
    gtk_widget_set_sensitive(GTK_WIDGET(m_rel_entry->get_widget()),
                             !m_use_absolute);
}

void
BothDateEntry::set_entry_from_option(GncOption& option)
{
    m_use_absolute =
        option.get_value<RelativeDatePeriod>() == RelativeDatePeriod::ABSOLUTE;
    if (m_use_absolute)
        m_abs_entry->set_entry_from_option(option);
    else
        m_rel_entry->set_entry_from_option(option);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_rel_button),
                                 !m_use_absolute);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_abs_button),
                                 m_use_absolute);

    toggle_relative(m_use_absolute);
}

void
BothDateEntry::set_option_from_entry(GncOption& option)
{
    if (m_use_absolute)
        m_abs_entry->set_option_from_entry(option);
    else
        m_rel_entry->set_option_from_entry(option);
}

void
BothDateEntry::block_signals(bool block)
{
    if (block)
    {
        g_signal_handler_block(m_abs_button, m_abs_hdlr);
        g_signal_handler_block(m_rel_button, m_rel_hdlr);
    }
    else
    {
        g_signal_handler_unblock(m_abs_button, m_abs_hdlr);
        g_signal_handler_unblock(m_rel_button, m_rel_hdlr);
    }
    m_abs_entry->block_signals(block);
    m_rel_entry->block_signals(block);
}

class GncOptionDateUIItem : public GncOptionGtkUIItem
{
public:
    GncOptionDateUIItem(GncDateEntryPtr entry, GncOptionUIType type) :
        GncOptionGtkUIItem{entry->get_widget(), type}, m_entry{std::move(entry)} { }
    ~GncOptionDateUIItem() = default;
    void clear_ui_item() override { m_entry = nullptr; }
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        if (m_entry)
            m_entry->set_entry_from_option(option);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        if (m_entry)
            m_entry->set_option_from_entry(option);
    }
    GtkWidget* get_widget() const override
    {
        return m_entry->get_widget();
    }
    GncDateEntry* get_entry() { return m_entry.get(); }
private:
    GncDateEntryPtr m_entry;
};

static void
date_set_absolute_cb(GtkWidget *widget, gpointer data1)
{
    GncOption* option = static_cast<decltype(option)>(data1);
    auto ui_item = option->get_ui_item();
    if (auto date_ui = dynamic_cast<const GncOptionDateUIItem* const>(ui_item))
    {
        const_cast<GncOptionDateUIItem*>(date_ui)->get_entry()->toggle_relative(true);
        gnc_option_changed_option_cb(widget, option);
    }
}

static void
date_set_relative_cb(GtkWidget *widget, gpointer data1)
{
    GncOption* option = static_cast<decltype(option)>(data1);
    auto ui_item = option->get_ui_item();
    if (auto date_ui = dynamic_cast<const GncOptionDateUIItem* const>(ui_item))
    {
        const_cast<GncOptionDateUIItem*>(date_ui)->get_entry()->toggle_relative(false);
        gnc_option_changed_option_cb(widget, option);
    }
}

static void
create_date_option_widget(GncOption& option, GtkGrid *page_box, int row)
{
    GtkWidget *enclosing{nullptr};
    auto type = option.get_ui_type();
    switch (type)
    {
        case GncOptionUIType::DATE_ABSOLUTE:
            option.set_ui_item(std::make_unique<GncOptionDateUIItem>(std::make_unique<AbsoluteDateEntry>(option), type));
            break;
        case GncOptionUIType::DATE_RELATIVE:
            option.set_ui_item(std::make_unique<GncOptionDateUIItem>(std::make_unique<RelativeDateEntry>(option), type));
            break;
        case GncOptionUIType::DATE_BOTH:
            option.set_ui_item(std::make_unique<GncOptionDateUIItem>(std::make_unique<BothDateEntry>(option), type));
            break;
        default:
            PERR("Attempted to create date option widget with wrong UI type %d",
                 static_cast<int>(type));
            break;
    }

    auto widget{option_get_gtk_widget(&option)};
    if (type == GncOptionUIType::DATE_RELATIVE)
    {
        enclosing = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
        gtk_box_set_homogeneous(GTK_BOX (enclosing), FALSE);

        gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
    }
    else
    {
        enclosing = gtk_frame_new(nullptr);
        g_object_set(G_OBJECT(widget), "margin", 3, NULL);

        gtk_container_add (GTK_CONTAINER(enclosing), widget);
    }

    gtk_widget_set_halign (GTK_WIDGET(enclosing), GTK_ALIGN_START);
    set_name_label(option, page_box, row, false);
    set_tool_tip(option, enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);

    auto ui_item{dynamic_cast<GncOptionDateUIItem*>(option.get_ui_item())};
    if (auto date_ui{ui_item ? ui_item->get_entry() : nullptr})
    {
        date_ui->block_signals(true);
        date_ui->set_entry_from_option(option);
        date_ui->block_signals(false);
    }

    gtk_widget_show_all(enclosing);
}

template<> void
create_option_widget<GncOptionUIType::DATE_ABSOLUTE>(GncOption& option,
                                                     GtkGrid *page_box, int row)
{
    create_date_option_widget(option, page_box, row);
}

template<> void
create_option_widget<GncOptionUIType::DATE_RELATIVE>(GncOption& option,
                                                     GtkGrid *page_box, int row)
{
    create_date_option_widget(option, page_box, row);
}

template<> void
create_option_widget<GncOptionUIType::DATE_BOTH>(GncOption& option,
                                                 GtkGrid *page_box, int row)
{
    create_date_option_widget(option, page_box, row);
}

using GncOptionAccountList = std::vector<GncGUID>;

static void
account_select_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GncTreeViewAccount *tree_view;
    GtkTreeSelection *selection;

    tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget (option));
    gtk_tree_view_expand_all(GTK_TREE_VIEW(tree_view));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    gtk_tree_selection_select_all(selection);
    gnc_option_changed_widget_cb(widget, option);
}

static void
account_clear_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GncTreeViewAccount *tree_view;
    GtkTreeSelection *selection;

    tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget (option));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    gtk_tree_selection_unselect_all(selection);
    gnc_option_changed_widget_cb(widget, option);
}

static void
account_select_children_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GncTreeViewAccount *tree_view;
    GList *acct_list = NULL, *acct_iter = NULL;

    tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget (option));
    acct_list = gnc_tree_view_account_get_selected_accounts (tree_view);

    for (acct_iter = acct_list; acct_iter; acct_iter = acct_iter->next)
        gnc_tree_view_account_select_subaccounts (tree_view, static_cast<Account*>(acct_iter->data));

    g_list_free (acct_list);
}

static void
account_set_default_cb(GtkWidget* widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    account_clear_all_cb(widget, data);
    option->set_value(option->get_default_value<GncOptionAccountList>());
    option->set_ui_item_from_option();
}

static void
show_hidden_toggled_cb(GtkWidget *widget, GncOption* option)
{
    if (option->get_ui_type() != GncOptionUIType::ACCOUNT_LIST &&
        option->get_ui_type() != GncOptionUIType::ACCOUNT_SEL)
        return;

    auto tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget(option));
    AccountViewInfo avi;
    gnc_tree_view_account_get_view_info (tree_view, &avi);
    avi.show_hidden = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
    gnc_tree_view_account_set_view_info (tree_view, &avi);
    gnc_option_changed_widget_cb(widget, option);
}

class GncGtkAccountListUIItem : public GncOptionGtkUIItem
{
public:
    explicit GncGtkAccountListUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::ACCOUNT_LIST} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_TREE_VIEW_ACCOUNT(get_widget())};
        GList *acc_list = nullptr;
        const GncOptionAccountList& accounts =
            option.get_value<GncOptionAccountList>();
        auto book{gnc_get_current_book()};
        for (auto guid : accounts)
        {
            auto account{xaccAccountLookup(&guid, book)};
            acc_list = g_list_prepend(acc_list, account);
        }
        acc_list = g_list_reverse(acc_list);
        gnc_tree_view_account_set_selected_accounts(widget, acc_list, TRUE);
        g_list_free(acc_list);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_TREE_VIEW_ACCOUNT(get_widget())};
        auto acc_list = gnc_tree_view_account_get_selected_accounts(widget);
        GncOptionAccountList acc_vec;
        acc_vec.reserve(g_list_length(acc_list));
        for (auto node = acc_list; node; node = g_list_next(node))
        {
            auto guid{qof_entity_get_guid(node->data)};
            acc_vec.push_back(*guid);
        }
        g_list_free(acc_list);
        option.set_value(acc_vec);
    }
};

static GtkWidget*
create_account_widget(GncOption& option, char *name)
{
    bool multiple_selection;
    GtkWidget *scroll_win;
    GtkWidget *button;
    GtkWidget *frame;
    GtkWidget *tree;
    GtkWidget *vbox;
    GtkWidget *bbox;
    GList *acct_type_list;
    GtkTreeSelection *selection;

    multiple_selection = option.is_multiselect();
    acct_type_list = option.account_type_list();

    frame = gtk_frame_new(name);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);

    gtk_container_add(GTK_CONTAINER(frame), vbox);

    tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(tree), FALSE);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
    if (multiple_selection)
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);
    else
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

    if (acct_type_list)
    {
        GList *node;
        AccountViewInfo avi;
        int i;

        gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

        for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
            avi.include_type[i] = FALSE;
        avi.show_hidden = TRUE;

        for (node = acct_type_list; node; node = node->next)
        {
            GNCAccountType type = static_cast<decltype(type)>(GPOINTER_TO_INT (node->data));
            if (type < NUM_ACCOUNT_TYPES)
                avi.include_type[type] = TRUE;
        }

        gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
        g_list_free (acct_type_list);
    }
    else
    {
        AccountViewInfo avi;
        int i;

        gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

        for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
            avi.include_type[i] = TRUE;
        avi.show_hidden = TRUE;
        gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
    }

    scroll_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);

    gtk_box_pack_start(GTK_BOX(vbox), scroll_win, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(scroll_win), 5);

    bbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
    gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
    gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 10);

    option.set_ui_item(std::make_unique<GncGtkAccountListUIItem>(tree));
    option.set_ui_item_from_option();

    if (multiple_selection)
    {
        button = gtk_button_new_with_label(_("Select All"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Select all accounts."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(account_select_all_cb), &option);

        button = gtk_button_new_with_label(_("Clear All"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Clear the selection and unselect all accounts."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(account_clear_all_cb), &option);

        button = gtk_button_new_with_label(_("Select Children"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Select all descendents of selected account."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(account_select_children_cb), &option);
    }

    button = gtk_button_new_with_label(_("Select Default"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select the default account selection."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(account_set_default_cb), &option);

    gtk_widget_set_margin_start (GTK_WIDGET(bbox), 6);
    gtk_widget_set_margin_end (GTK_WIDGET(bbox), 6);

    if (multiple_selection)
    {
        /* Put the "Show hidden" checkbox on a separate line since
           the 4 buttons make the dialog too wide. */
        bbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
        gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
        gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 0);
    }

    button = gtk_check_button_new_with_label(_("Show Hidden Accounts"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Show accounts that have been marked hidden."));
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), TRUE);
    g_signal_connect(G_OBJECT(button), "toggled",
                     G_CALLBACK(show_hidden_toggled_cb), &option);

    gtk_container_add(GTK_CONTAINER(scroll_win), tree);
    return frame;
}

static void
option_account_sel_changed_cb(GtkTreeSelection *sel, gpointer data)
{
    auto tree_view{gtk_tree_selection_get_tree_view(sel)};
    gnc_option_changed_widget_cb(GTK_WIDGET(tree_view),
                                 static_cast<GncOption*>(data));
}

template<> void
create_option_widget<GncOptionUIType::ACCOUNT_LIST>(GncOption& option,
                                                    GtkGrid *page_box, int row)
{
    auto enclosing{create_account_widget(option, nullptr)};
    gtk_widget_set_vexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(enclosing), TRUE);
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);

    auto widget{option_get_gtk_widget(&option)};
    auto selection{gtk_tree_view_get_selection(GTK_TREE_VIEW(widget))};
    g_signal_connect(G_OBJECT(selection), "changed",
                     G_CALLBACK(option_account_sel_changed_cb), &option);
    gtk_widget_show_all(enclosing);
}

class GncGtkAccountSelUIItem : public GncOptionGtkUIItem
{
public:
    explicit GncGtkAccountSelUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::ACCOUNT_SEL} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_ACCOUNT_SEL(get_widget())};
        auto instance{option.get_value<const Account*>()};
        if (instance)
            gnc_account_sel_set_account(widget, const_cast<Account*>(instance), FALSE);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_ACCOUNT_SEL(get_widget())};
// Must cast it to const Account* to get the template specialization to recognize it.
        option.set_value(static_cast<const Account*>(gnc_account_sel_get_account(widget)));
    }
};

template<> void
create_option_widget<GncOptionUIType::ACCOUNT_SEL> (GncOption& option,
                                                    GtkGrid *page_box, int row)
{
    auto acct_type_list{option.account_type_list()};
    auto widget{gnc_account_sel_new()};
    gnc_account_sel_set_acct_filters(GNC_ACCOUNT_SEL(widget),
                                     acct_type_list, NULL);
    g_list_free(acct_type_list);

    // gnc_account_sel doesn't emit a changed signal
    option.set_ui_item(std::make_unique<GncGtkAccountSelUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(widget, "account_sel_changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
    // wrap_widget sets the parent so this comes after.
    gtk_container_child_set(GTK_CONTAINER(gtk_widget_get_parent(widget)),
                            widget, "fill", TRUE, "expand", TRUE,
                            nullptr);
}

static void
list_changed_cb(GtkTreeSelection *selection, GncOption* option)
{
    GtkTreeView *view = GTK_TREE_VIEW(option_get_gtk_widget (option));
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
list_select_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GtkTreeView *view;
    GtkTreeSelection *selection;

    view = GTK_TREE_VIEW(option_get_gtk_widget(option));
    selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_select_all(selection);
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
list_clear_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GtkTreeView *view;
    GtkTreeSelection *selection;

    view = GTK_TREE_VIEW(option_get_gtk_widget(option));
    selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_unselect_all(selection);
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
list_set_default_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    list_clear_all_cb(widget, data);
    option->set_value(option->get_default_value<GncMultichoiceOptionIndexVec>());
    option->set_ui_item_from_option();
}

class GncGtkListUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkListUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::LIST} {}

    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_TREE_VIEW(get_widget())};
        auto selection{gtk_tree_view_get_selection(widget)};
        gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
        g_signal_handlers_block_by_func(selection, (gpointer)list_changed_cb, &option);
        gtk_tree_selection_unselect_all(selection);
        for (auto index : option.get_value<GncMultichoiceOptionIndexVec>())
        {
            auto path{gtk_tree_path_new_from_indices(index, -1)};
            gtk_tree_selection_select_path(selection, path);
            gtk_tree_path_free(path);
        }
        g_signal_handlers_unblock_by_func(selection, (gpointer)list_changed_cb, &option);
    }

    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_TREE_VIEW(get_widget())};
        auto selection{gtk_tree_view_get_selection(widget)};
        auto selected_rows{gtk_tree_selection_get_selected_rows(selection, nullptr)};
        GncMultichoiceOptionIndexVec vec;
        for (auto row = selected_rows; row; row = g_list_next(row))
        {
            auto path{static_cast<GtkTreePath*>(row->data)};
            auto indices{gtk_tree_path_get_indices(path)};
            vec.push_back(*indices);
        }
        g_list_free_full(selected_rows, (GDestroyNotify)gtk_tree_path_free);
        option.set_value(vec);
    }
};

static GtkWidget *
create_list_widget(GncOption& option, char *name)
{
    auto frame = gtk_frame_new(name);
    auto hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_container_add(GTK_CONTAINER(frame), hbox);

    auto store = gtk_list_store_new(1, G_TYPE_STRING);
    auto view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
    g_object_unref(store);
    auto renderer = gtk_cell_renderer_text_new();
    auto column = gtk_tree_view_column_new_with_attributes("", renderer,
                                                           "text", 0,
                                                           NULL);
    gtk_tree_view_append_column(view, column);
    gtk_tree_view_set_headers_visible(view, FALSE);

    auto num_values = option.num_permissible_values();
    for (decltype(num_values) i = 0; i < num_values; i++)
    {
        GtkTreeIter iter;
        auto raw_string = option.permissible_value_name(i);
        auto string = (raw_string && *raw_string) ? _(raw_string) : "";
        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter, 0, string ? string : "", -1);
    }

    option.set_ui_item(std::make_unique<GncGtkListUIItem>(GTK_WIDGET(view)));
    option.set_ui_item_from_option();

    gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(view), FALSE, FALSE, 0);

    auto selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(list_changed_cb), &option);

    auto bbox = gtk_button_box_new (GTK_ORIENTATION_VERTICAL);
    gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
    gtk_box_pack_end(GTK_BOX(hbox), bbox, FALSE, FALSE, 0);

    auto button = gtk_button_new_with_label(_("Select All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select all entries."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(list_select_all_cb), &option);

    button = gtk_button_new_with_label(_("Clear All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Clear the selection and unselect all entries."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(list_clear_all_cb), &option);

    button = gtk_button_new_with_label(_("Select Default"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select the default selection."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(list_set_default_cb), &option);

    g_object_set (G_OBJECT(hbox), "margin", 3, NULL);

    return frame;
}

template<> void
create_option_widget<GncOptionUIType::LIST> (GncOption& option,
                                             GtkGrid *page_box, int row)
{

    auto enclosing{create_list_widget(option, nullptr)};
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);
    gtk_widget_show(enclosing);
}

class GncGtkNumberRangeUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkNumberRangeUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::NUMBER_RANGE} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        double value;
        if (option.is_alternate())
            value = static_cast<double>(option.get_value<int>());
        else
            value = option.get_value<double>();

        gtk_spin_button_set_value(GTK_SPIN_BUTTON(get_widget()), value);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto value{gtk_spin_button_get_value(GTK_SPIN_BUTTON(get_widget()))};
        if (option.is_alternate())
            option.set_value<int>(static_cast<int>(value));
        else
            option.set_value<double>(value);
    }
};

/* New spin button configured with the values provided by the passed-in
 * GncOption, which had better contain a GncOptionRangeValue.
 *
 * Also used to set up the pixel spinner in the plot_size control.
 */
static GtkSpinButton*
create_range_spinner(GncOption& option)
{
    gdouble lower_bound = G_MINDOUBLE;
    gdouble upper_bound = G_MAXDOUBLE;
    gdouble step_size = 1.0;

    if (option.is_alternate())
    {
        int tmp_lower_bound = G_MININT;
        int tmp_upper_bound = G_MAXINT;
        int tmp_step_size = 1.0;
        option.get_limits<int>(tmp_upper_bound, tmp_lower_bound, tmp_step_size);
        lower_bound =(double)tmp_lower_bound;
        upper_bound = (double)tmp_upper_bound;
        step_size = (double)tmp_step_size;
    }
    else
        option.get_limits<double>(upper_bound, lower_bound, step_size);

    auto adj = GTK_ADJUSTMENT(gtk_adjustment_new(lower_bound, lower_bound,
                                                 upper_bound, step_size,
                                                 step_size * 5.0,
                                                 0));

    size_t num_decimals = 0;
    for (auto steps = step_size; steps < 1; steps *= 10)
        ++num_decimals;
    auto widget = gtk_spin_button_new(adj, step_size, num_decimals);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(widget), TRUE);

    size_t num_digits = 0;
    for (auto bigger = MAX(ABS(lower_bound), ABS(upper_bound));
         bigger >= 1; bigger /= 10.0)
        ++num_digits;
    num_digits += num_decimals;
    gtk_entry_set_width_chars(GTK_ENTRY(widget), num_digits);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget),
                              (upper_bound / 2)); //default
    return GTK_SPIN_BUTTON(widget);
}

template<> void
create_option_widget<GncOptionUIType::NUMBER_RANGE> (GncOption& option,
                                                     GtkGrid *page_box, int row)
{
    auto widget{create_range_spinner(option)};
    option.set_ui_item(std::make_unique<GncGtkNumberRangeUIItem>(GTK_WIDGET(widget)));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, GTK_WIDGET(widget), page_box, row);
}

class GncGtkColorUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkColorUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::COLOR} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        GdkRGBA color;
        /* gdk_rgba_parse uses pango_color_parse for hex color strings instead
         * of pango_color_parse_with_alpha and that fails if the string length
         * is 8.
        */
        auto value{option.get_value<std::string>().substr(0,6)};
        auto rgba_str{g_strdup_printf("#%s", value.c_str())};
        if (gdk_rgba_parse(&color, rgba_str))
        {
            auto color_button = GTK_COLOR_CHOOSER(get_widget());
            gtk_color_chooser_set_rgba(color_button, &color);
        }
        g_free(rgba_str);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        GdkRGBA color;
        auto color_button = GTK_COLOR_CHOOSER(get_widget());
        gtk_color_chooser_get_rgba(color_button, &color);
        auto rgba_str = g_strdup_printf("%2x%2x%2x%2x",
                                        (uint8_t)(color.red * 255),
                                        (uint8_t)(color.green * 255),
                                        (uint8_t)(color.blue * 255),
                                        (uint8_t)(color.alpha * 255));
        auto rgb_str = g_strdup_printf("%2x%2x%2x",
                                       (uint8_t)(color.red * 255),
                                       (uint8_t)(color.green * 255),
                                       (uint8_t)(color.blue * 255));
// sample-report.scm uses an old HTML4 attribute that doesn't understand alpha.
        option.set_value(std::string{rgb_str});
        g_free(rgba_str);
        g_free(rgb_str);
    }
};

template<> void
create_option_widget<GncOptionUIType::COLOR> (GncOption& option, GtkGrid *page_box, int row)
{
    auto widget = gtk_color_button_new();
    gtk_color_chooser_set_use_alpha(GTK_COLOR_CHOOSER(widget), TRUE);

    option.set_ui_item(std::make_unique<GncGtkColorUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "color-set",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}

class GncGtkFontUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkFontUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::FONT} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        GtkFontChooser *font_chooser = GTK_FONT_CHOOSER(get_widget());
        gtk_font_chooser_set_font(font_chooser,
                                  option.get_value<std::string>().c_str());

    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        GtkFontChooser *font_chooser = GTK_FONT_CHOOSER(get_widget());
        option.set_value(std::string{gtk_font_chooser_get_font(font_chooser)});
    }
};

template<> void
create_option_widget<GncOptionUIType::FONT> (GncOption& option, GtkGrid *page_box, int row)
{
    auto widget{gtk_font_button_new()};
    g_object_set(G_OBJECT(widget),
                 "use-font", TRUE,
                 "show-style", TRUE,
                 "show-size", TRUE,
                 (char *)NULL);

    option.set_ui_item(std::make_unique<GncGtkFontUIItem>(widget));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT(widget), "font-set",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}
/* A pointer to the last selected filename */
#define LAST_SELECTION "last-selection"

static void
update_preview_cb (GtkFileChooser *chooser, void* data)
{
    g_return_if_fail(chooser != NULL);

    ENTER("chooser %p", chooser);
    auto filename = gtk_file_chooser_get_preview_filename(chooser);
    DEBUG("chooser preview name is %s.", filename ? filename : "(null)");
    if (filename == NULL)
    {
        filename = g_strdup(static_cast<const char*>(g_object_get_data(G_OBJECT(chooser), LAST_SELECTION)));
        DEBUG("using last selection of %s", filename ? filename : "(null)");
        if (filename == NULL)
        {
            LEAVE("no usable name");
            return;
        }
    }

    auto image = GTK_IMAGE(gtk_file_chooser_get_preview_widget(chooser));
    auto pixbuf = gdk_pixbuf_new_from_file_at_size(filename, 128, 128, NULL);
    g_free(filename);
    auto have_preview = (pixbuf != NULL);

    gtk_image_set_from_pixbuf(image, pixbuf);
    if (pixbuf)
        g_object_unref(pixbuf);

    gtk_file_chooser_set_preview_widget_active(chooser, have_preview);
    LEAVE("preview visible is %d", have_preview);
}

static void
change_image_cb (GtkFileChooser *chooser, void* data)
{
    auto filename{gtk_file_chooser_get_preview_filename(chooser)};
    if (!filename)
        return;
    g_object_set_data_full(G_OBJECT(chooser), LAST_SELECTION, filename, g_free);
}

class GncGtkPixmapUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkPixmapUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::PIXMAP} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto string{option.get_value<std::string>()};
        if (!string.empty())
        {
            DEBUG("string = %s", string.c_str());
            auto chooser{GTK_FILE_CHOOSER(get_widget())};
            gtk_file_chooser_select_filename(chooser, string.c_str());
            auto filename{gtk_file_chooser_get_filename(chooser)};
            g_object_set_data_full(G_OBJECT(chooser), LAST_SELECTION,
                                   g_strdup(string.c_str()), g_free);
            DEBUG("Set %s, retrieved %s", string.c_str(),
                  filename ? filename : "(null)");
            update_preview_cb(chooser, &option);
        }
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto string = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(get_widget()));
        DEBUG("filename %s", string ? string : "(null)");
        if (string)
        {
            option.set_value(std::string{string});
            g_free(string);
        }
    }
};

template<> void
create_option_widget<GncOptionUIType::PIXMAP> (GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto enclosing{gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_box_set_homogeneous(GTK_BOX(enclosing), FALSE);
    auto button{gtk_button_new_with_label(_("Clear"))};
    gtk_widget_set_tooltip_text(button, _("Clear any selected image file."));
    auto widget{ gtk_file_chooser_button_new(_("Select image"),
                                             GTK_FILE_CHOOSER_ACTION_OPEN)};
    gtk_widget_set_tooltip_text(widget, _("Select an image file."));
    g_object_set(G_OBJECT(widget),
                 "width-chars", 30,
                 "preview-widget", gtk_image_new(),
                 (char *)NULL);
    option.set_ui_item(std::make_unique<GncGtkPixmapUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT (widget), "selection-changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    g_signal_connect(G_OBJECT (widget), "selection-changed",
                     G_CALLBACK(change_image_cb), &option);
    g_signal_connect(G_OBJECT (widget), "update-preview",
                     G_CALLBACK(update_preview_cb), &option);
    g_signal_connect_swapped(G_OBJECT (button), "clicked",
                             G_CALLBACK(gtk_file_chooser_unselect_all), widget);

    gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), button, FALSE, FALSE, 0);

    gtk_widget_show(widget);
    set_name_label(option, page_box, row, false);
    set_tool_tip(option, enclosing);
    gtk_widget_show(enclosing);
    grid_attach_widget(page_box, enclosing, row);
}

static void
radiobutton_set_cb(GtkWidget *w, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    gpointer _current, _new_value;
    gint current, new_value;

    auto widget = option_get_gtk_widget(option);

    _current = g_object_get_data(G_OBJECT(widget), "gnc_radiobutton_index");
    current = GPOINTER_TO_INT (_current);

    _new_value = g_object_get_data (G_OBJECT(w), "gnc_radiobutton_index");
    new_value = GPOINTER_TO_INT (_new_value);

    if (current == new_value)
        return;

    g_object_set_data (G_OBJECT(widget), "gnc_radiobutton_index",
                       GINT_TO_POINTER(new_value));
    gnc_option_changed_widget_cb(widget, option);
}

class GncGtkRadioButtonUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkRadioButtonUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::RADIOBUTTON} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto index{option.get_value<uint16_t>()};
        auto list{gtk_container_get_children(GTK_CONTAINER(get_widget()))};
        auto box{GTK_WIDGET(list->data)};
        g_list_free(list);

        list = gtk_container_get_children(GTK_CONTAINER(box));
        auto node{g_list_nth(list, index)};
        GtkButton* button{};
        if (node)
        {
            button = GTK_BUTTON(node->data);
        }
        else
        {
            PERR("Invalid Radio Button Selection %hu", index);
            g_list_free(list);
            return;
        }
        g_list_free(list);
        auto val{g_object_get_data (G_OBJECT (button),
                                    "gnc_radiobutton_index")};
        g_return_if_fail (GPOINTER_TO_UINT (val) == index);

        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto index{g_object_get_data(G_OBJECT(get_widget()),
                                     "gnc_radiobutton_index")};
        option.set_value<uint16_t>(GPOINTER_TO_INT(index));
    }
};

static GtkWidget *
create_radiobutton_widget(char *name, GncOption& option)
{
    GtkWidget *frame, *box;
    GtkWidget *widget = NULL;

    auto num_values{option.num_permissible_values()};

    g_return_val_if_fail(num_values >= 0, NULL);

    /* Create our button frame */
    frame = gtk_frame_new (name);

    /* Create the button box */
    box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);
    gtk_container_add (GTK_CONTAINER (frame), box);

    option.set_ui_item(std::make_unique<GncGtkPixmapUIItem>(frame));
    option.set_ui_item_from_option();

    /* Iterate over the options and create a radio button for each one */
    for (decltype(num_values) i = 0; i < num_values; i++)
    {
        auto label = option.permissible_value_name(i);

        widget =
            gtk_radio_button_new_with_label_from_widget (widget ?
                                                         GTK_RADIO_BUTTON (widget) :
                                                         NULL,
                                                         label && *label ? _(label) : "");
        g_object_set_data (G_OBJECT (widget), "gnc_radiobutton_index",
                           GINT_TO_POINTER (i));
        g_signal_connect(G_OBJECT(widget), "toggled",
                         G_CALLBACK(radiobutton_set_cb), &option);
        gtk_box_pack_start (GTK_BOX (box), widget, FALSE, FALSE, 0);
    }

    return frame;
}

template<> void
create_option_widget<GncOptionUIType::RADIOBUTTON> (GncOption& option, GtkGrid *page_box, int row)
 {
     auto enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
     gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
     set_name_label(option, page_box, row, true);
     set_tool_tip(option, enclosing);
     auto widget = create_radiobutton_widget(NULL, option);
     gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
     gtk_widget_show_all(enclosing);
     grid_attach_widget(page_box, enclosing, row);
 }

class GncGtkDateFormatUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkDateFormatUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::DATE_FORMAT} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_DATE_FORMAT(get_widget())};
        auto [format, months, years, custom] = option.get_value<GncOptionDateFormat>();
        gnc_date_format_set_format(widget, format);
        gnc_date_format_set_months(widget, months);
        gnc_date_format_set_years(widget, years);
        gnc_date_format_set_custom(widget, custom.c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_DATE_FORMAT(get_widget())};
        GncOptionDateFormat format{
            gnc_date_format_get_format(widget),
            gnc_date_format_get_months(widget),
            gnc_date_format_get_years(widget),
            gnc_date_format_get_custom(widget)};
        option.set_value(format);
    }
};


template<> void
create_option_widget<GncOptionUIType::DATE_FORMAT> (GncOption& option,
                                                    GtkGrid *page_box, int row)
{
    auto enclosing = gnc_date_format_new_without_label ();
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    option.set_ui_item(std::make_unique<GncGtkDateFormatUIItem>(enclosing));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(enclosing), "format_changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    gtk_widget_show_all(enclosing);
    grid_attach_widget(page_box, enclosing, row);
}

class PlotSize;
static void plot_size_set_pixels(GtkWidget*, PlotSize*);
static void plot_size_set_percent(GtkWidget*, PlotSize*);

class PlotSize
{
    GtkWidget *m_widget;
    GtkWidget *m_pixel_button;
    GtkWidget *m_percent_button;
    GtkWidget *m_range_spinner;
    GtkAdjustment *m_adj_pct;
    GtkAdjustment *m_adj_px;
    unsigned long m_percent_handler;
    unsigned long m_pixel_handler;
public:
    PlotSize(GncOption& option);
    ~PlotSize();
    void set_entry_from_option(GncOption& option);
    void set_option_from_entry(GncOption& option);
    GtkWidget* get_widget() { return m_widget; }
    GtkWidget* get_spinner() { return m_range_spinner; }
    void set_pixels() { gtk_spin_button_set_adjustment(GTK_SPIN_BUTTON(m_range_spinner), m_adj_px); }
    void set_percent() { gtk_spin_button_set_adjustment(GTK_SPIN_BUTTON(m_range_spinner), m_adj_pct); }
};

PlotSize::PlotSize(GncOption& option) :
    m_widget{gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 4)}, m_pixel_button{gtk_radio_button_new_with_label(nullptr, _("Pixels"))},
    m_percent_button{gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(m_pixel_button), _("Percent"))},
    m_range_spinner{GTK_WIDGET(create_range_spinner(option))},
    m_adj_pct{GTK_ADJUSTMENT(g_object_ref(gtk_adjustment_new(100.0, 10.0, 100.0, 1.0, 5.0, 0.0)))},
    m_adj_px{GTK_ADJUSTMENT(g_object_ref(gtk_adjustment_new(1000.0, 110.0, 10000.0, 10.0, 250.0, 0.0)))}
{
    gtk_box_set_homogeneous(GTK_BOX(m_widget), FALSE);
    g_object_set (G_OBJECT(m_widget), "margin", 3, NULL);
    set_tool_tip(option, m_widget);
    gtk_box_pack_start(GTK_BOX(m_widget), GTK_WIDGET(m_pixel_button), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget), GTK_WIDGET(m_percent_button), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget), GTK_WIDGET(m_range_spinner),
                       FALSE, FALSE, 0);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(m_pixel_button), FALSE);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(m_percent_button), TRUE);

    m_pixel_handler = g_signal_connect(m_pixel_button, "toggled", G_CALLBACK(plot_size_set_pixels), this);
    m_percent_handler = g_signal_connect(m_percent_button, "toggled", G_CALLBACK(plot_size_set_percent), this);
}

PlotSize::~PlotSize()
{
    g_signal_handler_disconnect(m_pixel_button, m_pixel_handler);
    g_signal_handler_disconnect(m_percent_button, m_percent_handler);
    g_object_unref(m_adj_pct);
    g_object_unref(m_adj_px);
}

void
PlotSize::set_option_from_entry(GncOption& option)
{
    auto value{gtk_spin_button_get_value(GTK_SPIN_BUTTON(m_range_spinner))};
    if (option.is_alternate())
        option.set_value<int>(static_cast<int>(value));
    else
        option.set_value<double>(value);
}

void
PlotSize::set_entry_from_option(GncOption& option)
{
    double value;
    if (option.is_alternate())
    {
        auto int_value{option.get_value<int>()};
        value = static_cast<double>(int_value);
    }
    else
    {
        value = option.get_value<double>();
    }

    if (value > 100.0)
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_pixel_button), TRUE);
    else
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_percent_button), TRUE);

    gtk_spin_button_set_value(GTK_SPIN_BUTTON(m_range_spinner), value);
}

void
plot_size_set_pixels(GtkWidget *widget, PlotSize *ps)
{
    ps->set_pixels();
}

void
plot_size_set_percent(GtkWidget *widget, PlotSize *ps)
{
    ps->set_percent();
}

using PlotSizePtr = std::unique_ptr<PlotSize>;

class GncGtkPlotSizeUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkPlotSizeUIItem(PlotSizePtr&& plot_size) :
        GncOptionGtkUIItem{plot_size->get_widget(), GncOptionUIType::PLOT_SIZE},
        m_plot_size{std::move(plot_size)} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        m_plot_size->set_entry_from_option(option);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        m_plot_size->set_option_from_entry(option);
    }
    PlotSize* get_plot_size() { return m_plot_size.get(); }
private:
PlotSizePtr m_plot_size;
};

template<> void
create_option_widget<GncOptionUIType::PLOT_SIZE> (GncOption& option,
                                                  GtkGrid *page_box, int row)
{

    auto enclosing = gtk_frame_new(NULL);
    gtk_widget_set_halign (GTK_WIDGET(enclosing), GTK_ALIGN_START);
    set_name_label(option, page_box, row, false);

    option.set_ui_item(std::make_unique<GncGtkPlotSizeUIItem>(std::make_unique<PlotSize>(option)));
    option.set_ui_item_from_option();

    auto widget{option_get_gtk_widget(&option)};
    gtk_container_add(GTK_CONTAINER(enclosing), widget);

    gtk_widget_show_all(enclosing);
    grid_attach_widget(page_box, enclosing, row);

    auto ui_item{dynamic_cast<GncGtkPlotSizeUIItem*>(option.get_ui_item())};
    if (ui_item)
        g_signal_connect(G_OBJECT(ui_item->get_plot_size()->get_spinner()), "changed",
                         G_CALLBACK(gnc_option_changed_widget_cb), &option);
}

static GtkWidget *
create_budget_widget(GncOption& option)
{
    GtkTreeModel *tm;
    GtkComboBox *cb;
    GtkCellRenderer *cr;

    tm = gnc_tree_model_budget_new(gnc_get_current_book());
    cb = GTK_COMBO_BOX(gtk_combo_box_new_with_model(tm));
    g_object_unref(tm);
    cr = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(cb), cr, TRUE);

    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(cb), cr, "text",
                                   BUDGET_NAME_COLUMN, NULL);
    return GTK_WIDGET(cb);
}

class GncGtkBudgetUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkBudgetUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::BUDGET} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        GtkTreeIter iter;
        auto widget{GTK_COMBO_BOX(get_widget())};
        auto instance{option.get_value<const QofInstance*>()};
        if (instance)
        {
            auto tree_model{gtk_combo_box_get_model(widget)};
            if (gnc_tree_model_budget_get_iter_for_budget(tree_model, &iter,
                                                          GNC_BUDGET(instance)))
                gtk_combo_box_set_active_iter(widget, &iter);
        }
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        GtkTreeIter iter;
        auto widget{GTK_COMBO_BOX(get_widget())};
        if (gtk_combo_box_get_active_iter(widget, &iter))
        {
            auto tree_model{gtk_combo_box_get_model(widget)};
            auto budget{gnc_tree_model_budget_get_budget(tree_model, &iter)};
            option.set_value(qof_instance_cast(budget));
        }
    }
};

template<> void
create_option_widget<GncOptionUIType::BUDGET> (GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto widget{create_budget_widget(option)};

    option.set_ui_item(std::make_unique<GncGtkBudgetUIItem>(widget));
    option.set_ui_item_from_option();

    /* Maybe connect destroy handler for tree model here? */
    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);

    wrap_widget(option, widget, page_box, row);
}

namespace {

enum AllocationColumns
{
    ALLOC_COL_GUID = 0,
    ALLOC_COL_NAME,
    ALLOC_COL_DIRECT_VAL,
    ALLOC_COL_DIRECT_TEXT,
    ALLOC_COL_SUBTREE_VAL,
    ALLOC_COL_SUBTREE_TEXT,
    ALLOC_COL_MACRO_NAME,
    ALLOC_COL_MACRO_EFF_TEXT,
    ALLOC_COL_WEIGHT,
    NUM_ALLOC_COLS
};

enum MacroClassColumns
{
    MACRO_COL_NAME = 0,
    MACRO_COL_EXPLICIT_VAL,
    MACRO_COL_EXPLICIT_TEXT,
    MACRO_COL_ACCTS_SUM_VAL,
    MACRO_COL_ACCTS_SUM_TEXT,
    MACRO_COL_EFFECTIVE_VAL,
    MACRO_COL_EFFECTIVE_TEXT,
    MACRO_COL_ACCOUNTS_DESC,
    NUM_MACRO_COLS
};

std::string
account_guid_str(const Account* acc)
{
    char guid_buf[GUID_ENCODING_LENGTH + 1];
    guid_to_string_buff(xaccAccountGetGUID(acc), guid_buf);
    return std::string{guid_buf};
}

std::string
trim_ws(const std::string& s)
{
    auto first = s.find_first_not_of(" \t\r\n");
    if (first == std::string::npos)
        return {};
    auto last = s.find_last_not_of(" \t\r\n");
    return s.substr(first, last - first + 1);
}

std::string
escape_alloc_token(const std::string& s)
{
    std::string out;
    out.reserve(s.size());
    for (char c : s)
    {
        switch (c)
        {
            case '%': out += "%25"; break;
            case ';': out += "%3B"; break;
            case '=': out += "%3D"; break;
            case '|': out += "%7C"; break;
            default:  out.push_back(c); break;
        }
    }
    return out;
}

std::string
unescape_alloc_token(const std::string& s)
{
    std::string out;
    out.reserve(s.size());
    for (size_t i = 0; i < s.size(); ++i)
    {
        if (s[i] == '%' && i + 2 < s.size())
        {
            auto hex = s.substr(i + 1, 2);
            if (hex == "25") { out.push_back('%'); i += 2; continue; }
            if (hex == "3B" || hex == "3b") { out.push_back(';'); i += 2; continue; }
            if (hex == "3D" || hex == "3d") { out.push_back('='); i += 2; continue; }
            if (hex == "7C" || hex == "7c") { out.push_back('|'); i += 2; continue; }
        }
        out.push_back(s[i]);
    }
    return out;
}

struct ParsedAllocations
{
    std::map<std::string, double> direct;
    std::map<std::string, std::string> macros;
};

ParsedAllocations
parse_allocation_entries(const std::string& raw)
{
    ParsedAllocations parsed;
    std::istringstream iss{raw};
    std::string token;
    while (std::getline(iss, token, ';'))
    {
        auto eq_pos = token.find('=');
        if (eq_pos == std::string::npos || eq_pos == 0)
            continue;
        std::string guid = trim_ws(token.substr(0, eq_pos));
        if (guid.empty())
            continue;
        std::string rhs = token.substr(eq_pos + 1);
        auto pipe_pos = rhs.find('|');
        std::string val_str = trim_ws(
            pipe_pos == std::string::npos ? rhs : rhs.substr(0, pipe_pos));
        std::string macro_name =
            pipe_pos == std::string::npos
                ? std::string{}
                : trim_ws(unescape_alloc_token(rhs.substr(pipe_pos + 1)));

        if (!val_str.empty())
        {
            char* endptr = nullptr;
            double val = g_ascii_strtod(val_str.c_str(), &endptr);
            if (endptr && *endptr == '\0' && std::isfinite(val) && val > 1e-6)
                parsed.direct[guid] = val;
        }
        if (!macro_name.empty())
            parsed.macros[guid] = macro_name;
    }
    return parsed;
}

std::string
format_allocation_number(double val)
{
    std::ostringstream oss;
    oss.imbue(std::locale::classic());
    double rounded4 = std::round(val * 10000.0) / 10000.0;
    double rounded2 = std::round(val * 100.0) / 100.0;
    if (std::fabs(rounded4 - rounded2) > 1e-4)
    {
        oss << std::fixed << std::setprecision(4) << rounded4;
        std::string s = oss.str();
        while (s.size() > 1 && s.back() == '0')
            s.pop_back();
        if (!s.empty() && s.back() == '.')
            s.pop_back();
        return s;
    }
    oss << std::fixed << std::setprecision(2) << rounded2;
    return oss.str();
}

std::string
serialize_allocation_entries(const std::map<std::string, double>& allocs,
                             const std::map<std::string, std::string>& macros)
{
    std::ostringstream oss;
    oss.imbue(std::locale::classic());
    std::map<std::string, std::pair<double, std::string>> merged;
    for (const auto& [guid, val] : allocs)
    {
        if (val > 1e-6)
            merged[guid].first = val;
    }
    for (const auto& [guid, macro] : macros)
    {
        if (!macro.empty())
            merged[guid].second = macro;
    }

    bool first = true;
    for (const auto& [guid, entry] : merged)
    {
        if (entry.first <= 1e-6 && entry.second.empty())
            continue;
        if (!first)
            oss << ';';
        first = false;
        oss << guid << '=' << format_allocation_number(entry.first);
        if (!entry.second.empty())
            oss << '|' << escape_alloc_token(entry.second);
    }
    return oss.str();
}

std::vector<std::pair<std::string, double>>
parse_macro_classes_string(const std::string& raw)
{
    std::vector<std::pair<std::string, double>> result;
    std::istringstream iss{raw};
    std::string token;
    while (std::getline(iss, token, ';'))
    {
        auto eq_pos = token.find('=');
        std::string raw_name = (eq_pos == std::string::npos)
                                   ? token
                                   : token.substr(0, eq_pos);
        std::string name = trim_ws(unescape_alloc_token(raw_name));
        if (name.empty())
            continue;
        double val = 0.0;
        if (eq_pos != std::string::npos)
        {
            std::string val_str = trim_ws(token.substr(eq_pos + 1));
            if (!val_str.empty())
            {
                char* endptr = nullptr;
                double parsed = g_ascii_strtod(val_str.c_str(), &endptr);
                if (endptr && *endptr == '\0' && std::isfinite(parsed) &&
                    parsed > 1e-6 && parsed <= 100.0)
                    val = parsed;
            }
        }
        bool dup = std::any_of(result.begin(), result.end(),
                               [&name](const auto& p) { return p.first == name; });
        if (!dup)
            result.emplace_back(std::move(name), val);
    }
    return result;
}

std::string
serialize_macro_classes(const std::vector<std::pair<std::string, double>>& list)
{
    std::ostringstream oss;
    oss.imbue(std::locale::classic());
    bool first = true;
    for (const auto& [name, val] : list)
    {
        if (name.empty())
            continue;
        if (!first)
            oss << ';';
        first = false;
        oss << escape_alloc_token(name) << '=' << format_allocation_number(val);
    }
    return oss.str();
}

bool
parse_user_percentage(const char* text, double& out_val)
{
    if (!text)
    {
        out_val = 0.0;
        return true;
    }
    std::string s{text};
    auto first = s.find_first_not_of(" \t\r\n");
    if (first == std::string::npos)
    {
        out_val = 0.0;
        return true;
    }
    auto last = s.find_last_not_of(" \t\r\n%");
    if (last == std::string::npos || last < first)
    {
        out_val = 0.0;
        return true;
    }
    s = s.substr(first, last - first + 1);
    std::replace(s.begin(), s.end(), ',', '.');
    char* endptr = nullptr;
    double val = g_ascii_strtod(s.c_str(), &endptr);
    if (!endptr || *endptr != '\0' || !std::isfinite(val))
        return false;
    if (val < 0.0 || val > 100.0)
        return false;
    out_val = val;
    return true;
}

std::string
format_pct_edit(double val)
{
    if (val <= 1e-6)
        return {};
    return format_allocation_number(val);
}

std::string
format_pct_display(double val)
{
    if (val <= 1e-6)
        return {};
    return format_allocation_number(val) + "%";
}

bool
is_allocation_account_type(GNCAccountType type)
{
    switch (type)
    {
        case ACCT_TYPE_ASSET:
        case ACCT_TYPE_BANK:
        case ACCT_TYPE_CASH:
        case ACCT_TYPE_STOCK:
        case ACCT_TYPE_MUTUAL:
            return true;
        default:
            return false;
    }
}

bool
account_should_show(Account* acc, bool show_hidden,
                    const std::map<std::string, double>& allocs,
                    const std::map<std::string, std::string>& macros)
{
    if (!acc)
        return false;
    const bool is_hidden = xaccAccountGetHidden(acc);
    const std::string guid = account_guid_str(acc);
    auto it = allocs.find(guid);
    const bool has_direct_alloc = (it != allocs.end() && it->second > 1e-6);
    auto mit = macros.find(guid);
    const bool has_macro = (mit != macros.end() && !mit->second.empty());

    bool child_visible = false;
    GList* children = gnc_account_get_children_sorted(acc);
    for (GList* n = children; n; n = n->next)
    {
        if (account_should_show(static_cast<Account*>(n->data), show_hidden,
                                allocs, macros))
        {
            child_visible = true;
            break;
        }
    }
    g_list_free(children);

    if (has_direct_alloc || has_macro || child_visible)
        return true;
    if (is_hidden && !show_hidden)
        return false;
    return is_allocation_account_type(xaccAccountGetType(acc));
}

struct MacroAccountSummary
{
    double accounts_sum = 0.0;
    std::vector<std::string> account_names;
};

struct AllocationTotalsSnapshot
{
    double outside_macros_direct = 0.0;
    std::map<std::string, MacroAccountSummary> summaries;
};

} // namespace

class GncGtkAccountAllocationUIItem;
class GncGtkMacroClassesUIItem;

namespace {
std::vector<GncGtkAccountAllocationUIItem*> s_alloc_ui_items;
std::vector<GncGtkMacroClassesUIItem*> s_macro_ui_items;

GtkWindow*
widget_toplevel_window(GtkWidget* w)
{
    if (!w)
        return nullptr;
    auto top = gtk_widget_get_toplevel(w);
    return (top && GTK_IS_WINDOW(top)) ? GTK_WINDOW(top) : nullptr;
}
} // namespace

class GncGtkMacroClassesUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkMacroClassesUIItem(GtkWidget* tree_view, GtkListStore* store,
                             GtkLabel* status_label,
                             GtkTreeViewColumn* name_col,
                             GncOption* option);
    ~GncGtkMacroClassesUIItem() override;

    void set_ui_item_from_option(GncOption& option) noexcept override;
    void set_option_from_ui_item(GncOption& option) noexcept override;

    const std::vector<std::pair<std::string, double>>& get_macro_list() const
    {
        return m_macros;
    }

    void update_account_summaries(
        const std::map<std::string, MacroAccountSummary>& summaries,
        double outside_macros_direct);
    void on_name_cell_edited(const gchar* path_str, const gchar* new_text,
                             GncOption* option);
    void on_explicit_cell_edited(const gchar* path_str, const gchar* new_text,
                                 GncOption* option);
    void on_add_clicked(GncOption* option);
    void on_remove_clicked(GncOption* option);
    void on_clear_targets_clicked(GncOption* option);
    void on_hierarchy_changed();

private:
    void populate_list();
    void update_status_label();
    std::string make_unique_macro_name() const;
    void notify_alloc_peer(const std::string& renamed_from = "",
                           const std::string& renamed_to = "");

    GtkListStore* m_store;
    GtkLabel* m_status_label;
    GtkTreeViewColumn* m_name_col;
    GncOption* m_option;
    std::vector<std::pair<std::string, double>> m_macros;
    std::map<std::string, MacroAccountSummary> m_summaries;
    double m_outside_macros_direct{0.0};
    bool m_syncing{false};
};

class GncGtkAccountAllocationUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkAccountAllocationUIItem(GtkWidget* tree_view, GtkTreeStore* store,
                                  GtkListStore* combo_store,
                                  GtkLabel* status_label,
                                  GtkToggleButton* show_hidden,
                                  GncOption* option) :
        GncOptionGtkUIItem{tree_view, GncOptionUIType::ACCOUNT_ALLOCATION},
        m_store{store},
        m_combo_store{GTK_LIST_STORE(g_object_ref(combo_store))},
        m_status_label{status_label},
        m_show_hidden{show_hidden},
        m_option{option}
    {
        s_alloc_ui_items.push_back(this);
    }

    ~GncGtkAccountAllocationUIItem() override
    {
        s_alloc_ui_items.erase(
            std::remove(s_alloc_ui_items.begin(), s_alloc_ui_items.end(), this),
            s_alloc_ui_items.end());
        if (m_combo_store)
            g_object_unref(m_combo_store);
    }

    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto parsed = parse_allocation_entries(option.get_value<std::string>());
        m_allocations = std::move(parsed.direct);
        m_account_macros = std::move(parsed.macros);
        populate_tree();
        notify_macro_peer();
    }

    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        option.set_value(serialize_allocation_entries(m_allocations, m_account_macros));
    }

    void on_direct_cell_edited(const gchar* path_str, const gchar* new_text,
                               GncOption* option)
    {
        GtkTreeIter iter;
        auto model = GTK_TREE_MODEL(m_store);
        if (!gtk_tree_model_get_iter_from_string(model, &iter, path_str))
            return;

        double val = 0.0;
        if (!parse_user_percentage(new_text, val))
            return;

        gchar* guid = nullptr;
        gtk_tree_model_get(model, &iter, ALLOC_COL_GUID, &guid, -1);
        if (!guid)
            return;

        if (val > 1e-6)
            m_allocations[guid] = val;
        else
            m_allocations.erase(guid);
        g_free(guid);

        auto edit_text = format_pct_edit(val);
        gtk_tree_store_set(m_store, &iter,
                           ALLOC_COL_DIRECT_VAL, val,
                           ALLOC_COL_DIRECT_TEXT, edit_text.c_str(),
                           -1);
        recompute_totals();
        notify_macro_peer();
        gnc_option_changed_widget_cb(get_widget(), option);
    }

    void on_macro_cell_edited(const gchar* path_str, const gchar* new_text,
                              GncOption* option)
    {
        GtkTreeIter iter;
        auto model = GTK_TREE_MODEL(m_store);
        if (!gtk_tree_model_get_iter_from_string(model, &iter, path_str))
            return;

        std::string selected = new_text ? trim_ws(new_text) : std::string{};
        if (selected == _("(None)") || selected == "(None)")
            selected.clear();

        gchar* guid = nullptr;
        gtk_tree_model_get(model, &iter, ALLOC_COL_GUID, &guid, -1);
        if (!guid)
            return;

        std::string prev;
        if (auto it = m_account_macros.find(guid); it != m_account_macros.end())
            prev = it->second;

        if (prev == selected)
        {
            g_free(guid);
            return;
        }

        if (!selected.empty())
            m_account_macros[guid] = selected;
        else
            m_account_macros.erase(guid);
        g_free(guid);

        gtk_tree_store_set(m_store, &iter,
                           ALLOC_COL_MACRO_NAME, selected.c_str(),
                           -1);
        recompute_totals();
        notify_macro_peer();
        refresh_macro_effective_cells();
        gnc_option_changed_widget_cb(get_widget(), option);
    }

    void on_clear_all(GncOption* option)
    {
        m_allocations.clear();
        m_account_macros.clear();
        populate_tree();
        notify_macro_peer();
        gnc_option_changed_widget_cb(get_widget(), option);
    }

    void on_show_hidden_toggled()
    {
        populate_tree();
    }

    void on_hierarchy_changed()
    {
        if (m_syncing)
            return;
        auto peer = find_macro_peer();
        if (peer)
            sync_from_macro_peer(peer->get_macro_list());
    }

    AllocationTotalsSnapshot compute_allocation_snapshot() const
    {
        AllocationTotalsSnapshot snapshot;
        auto model = GTK_TREE_MODEL(m_store);
        GtkTreeIter root_iter;
        if (gtk_tree_model_get_iter_first(model, &root_iter))
        {
            do
            {
                collect_allocation_snapshot(&root_iter, "", snapshot);
            } while (gtk_tree_model_iter_next(model, &root_iter));
        }
        return snapshot;
    }

    void sync_from_macro_peer(
        const std::vector<std::pair<std::string, double>>& macro_list,
        const std::string& renamed_from = "",
        const std::string& renamed_to = "")
    {
        if (m_syncing)
            return;
        m_syncing = true;

        bool changed = false;
        if (!renamed_from.empty() && !renamed_to.empty() && renamed_from != renamed_to)
        {
            for (auto& [guid, macro] : m_account_macros)
            {
                if (macro == renamed_from)
                {
                    macro = renamed_to;
                    changed = true;
                }
            }
        }

        /* Rebuild dropdown list model */
        gtk_list_store_clear(m_combo_store);
        GtkTreeIter combo_iter;
        gtk_list_store_append(m_combo_store, &combo_iter);
        gtk_list_store_set(m_combo_store, &combo_iter, 0, _("(None)"), -1);
        for (const auto& [name, val] : macro_list)
        {
            gtk_list_store_append(m_combo_store, &combo_iter);
            gtk_list_store_set(m_combo_store, &combo_iter, 0, name.c_str(), -1);
        }

        /* Remove assignments pointing to deleted macro classes */
        for (auto it = m_account_macros.begin(); it != m_account_macros.end();)
        {
            bool exists = std::any_of(macro_list.begin(), macro_list.end(),
                                      [&](const auto& p) { return p.first == it->second; });
            if (!exists)
            {
                it = m_account_macros.erase(it);
                changed = true;
            }
            else
                ++it;
        }

        update_tree_macro_names_from_map();
        auto snapshot = compute_allocation_snapshot();
        m_macro_explicit_cache.clear();
        m_macro_effective_cache.clear();
        for (const auto& [name, explicit_val] : macro_list)
        {
            m_macro_explicit_cache[name] = explicit_val;
            double acct_sum = 0.0;
            if (auto sit = snapshot.summaries.find(name); sit != snapshot.summaries.end())
                acct_sum = sit->second.accounts_sum;
            m_macro_effective_cache[name] =
                (explicit_val > 1e-6) ? explicit_val : acct_sum;
        }
        recompute_totals();
        refresh_macro_effective_cells();

        if (auto peer = find_macro_peer())
            peer->update_account_summaries(snapshot.summaries,
                                           snapshot.outside_macros_direct);

        if (changed && m_option)
            gnc_option_changed_widget_cb(get_widget(), m_option);

        m_syncing = false;
    }

private:
    GncGtkMacroClassesUIItem* find_macro_peer() const
    {
        auto top = widget_toplevel_window(get_widget());
        if (!top)
            return nullptr;
        for (auto* peer : s_macro_ui_items)
        {
            if (widget_toplevel_window(peer->get_widget()) == top)
                return peer;
        }
        return nullptr;
    }

    void notify_macro_peer()
    {
        if (m_syncing)
            return;
        auto peer = find_macro_peer();
        if (!peer)
            return;
        m_syncing = true;
        auto snapshot = compute_allocation_snapshot();
        const auto& macro_list = peer->get_macro_list();
        m_macro_explicit_cache.clear();
        m_macro_effective_cache.clear();
        for (const auto& [name, explicit_val] : macro_list)
        {
            m_macro_explicit_cache[name] = explicit_val;
            double acct_sum = 0.0;
            if (auto sit = snapshot.summaries.find(name); sit != snapshot.summaries.end())
                acct_sum = sit->second.accounts_sum;
            m_macro_effective_cache[name] =
                (explicit_val > 1e-6) ? explicit_val : acct_sum;
        }
        recompute_totals();
        refresh_macro_effective_cells();
        peer->update_account_summaries(snapshot.summaries,
                                       snapshot.outside_macros_direct);
        m_syncing = false;
    }

    void collect_allocation_snapshot(
        GtkTreeIter* iter,
        const std::string& inherited_macro,
        AllocationTotalsSnapshot& snapshot) const
    {
        auto model = GTK_TREE_MODEL(m_store);
        gchar* guid_c = nullptr;
        gchar* name_c = nullptr;
        double direct = 0.0;
        gtk_tree_model_get(model, iter,
                           ALLOC_COL_GUID, &guid_c,
                           ALLOC_COL_NAME, &name_c,
                           ALLOC_COL_DIRECT_VAL, &direct,
                           -1);
        std::string guid = guid_c ? guid_c : "";
        std::string acct_name = name_c ? name_c : "";
        g_free(guid_c);
        g_free(name_c);

        std::string own_macro;
        if (auto it = m_account_macros.find(guid); it != m_account_macros.end())
            own_macro = it->second;

        std::string eff_macro = !own_macro.empty() ? own_macro : inherited_macro;
        if (eff_macro.empty())
        {
            snapshot.outside_macros_direct += direct;
        }
        else
        {
            snapshot.summaries[eff_macro].accounts_sum += direct;
            if (!own_macro.empty() && own_macro != inherited_macro && !acct_name.empty())
                snapshot.summaries[eff_macro].account_names.push_back(acct_name);
        }

        GtkTreeIter child;
        if (gtk_tree_model_iter_children(model, &child, iter))
        {
            do
            {
                collect_allocation_snapshot(&child, eff_macro, snapshot);
            } while (gtk_tree_model_iter_next(model, &child));
        }
    }

    void update_tree_macro_names_subtree(GtkTreeIter* iter)
    {
        auto model = GTK_TREE_MODEL(m_store);
        gchar* guid_c = nullptr;
        gtk_tree_model_get(model, iter, ALLOC_COL_GUID, &guid_c, -1);
        std::string macro;
        if (guid_c)
        {
            if (auto it = m_account_macros.find(guid_c); it != m_account_macros.end())
                macro = it->second;
            g_free(guid_c);
        }
        gtk_tree_store_set(m_store, iter, ALLOC_COL_MACRO_NAME, macro.c_str(), -1);

        GtkTreeIter child;
        if (gtk_tree_model_iter_children(model, &child, iter))
        {
            do
            {
                update_tree_macro_names_subtree(&child);
            } while (gtk_tree_model_iter_next(model, &child));
        }
    }

    void update_tree_macro_names_from_map()
    {
        auto model = GTK_TREE_MODEL(m_store);
        GtkTreeIter root_iter;
        if (gtk_tree_model_get_iter_first(model, &root_iter))
        {
            do
            {
                update_tree_macro_names_subtree(&root_iter);
            } while (gtk_tree_model_iter_next(model, &root_iter));
        }
    }

    void refresh_macro_effective_subtree(GtkTreeIter* iter)
    {
        auto model = GTK_TREE_MODEL(m_store);
        gchar* macro_c = nullptr;
        gtk_tree_model_get(model, iter, ALLOC_COL_MACRO_NAME, &macro_c, -1);
        std::string macro = macro_c ? macro_c : "";
        g_free(macro_c);

        std::string eff_text;
        if (!macro.empty())
        {
            if (auto it = m_macro_effective_cache.find(macro);
                it != m_macro_effective_cache.end())
                eff_text = format_pct_display(it->second);
        }
        gtk_tree_store_set(m_store, iter,
                           ALLOC_COL_MACRO_EFF_TEXT, eff_text.c_str(),
                           -1);

        GtkTreeIter child;
        if (gtk_tree_model_iter_children(model, &child, iter))
        {
            do
            {
                refresh_macro_effective_subtree(&child);
            } while (gtk_tree_model_iter_next(model, &child));
        }
    }

    void refresh_macro_effective_cells()
    {
        auto model = GTK_TREE_MODEL(m_store);
        GtkTreeIter root_iter;
        if (gtk_tree_model_get_iter_first(model, &root_iter))
        {
            do
            {
                refresh_macro_effective_subtree(&root_iter);
            } while (gtk_tree_model_iter_next(model, &root_iter));
        }
    }

    double recompute_subtree(GtkTreeIter* iter)
    {
        auto model = GTK_TREE_MODEL(m_store);
        double direct = 0.0;
        gtk_tree_model_get(model, iter, ALLOC_COL_DIRECT_VAL, &direct, -1);

        double children_sum = 0.0;
        GtkTreeIter child;
        bool has_children = gtk_tree_model_iter_children(model, &child, iter);
        if (has_children)
        {
            do
            {
                children_sum += recompute_subtree(&child);
            } while (gtk_tree_model_iter_next(model, &child));
        }

        double subtree = direct + children_sum;
        auto subtree_text = format_pct_display(subtree);
        int weight = (has_children && subtree > 1e-6)
                         ? PANGO_WEIGHT_BOLD
                         : PANGO_WEIGHT_NORMAL;

        gtk_tree_store_set(m_store, iter,
                           ALLOC_COL_SUBTREE_VAL, subtree,
                           ALLOC_COL_SUBTREE_TEXT, subtree_text.c_str(),
                           ALLOC_COL_WEIGHT, weight,
                           -1);
        return subtree;
    }

    void recompute_totals()
    {
        auto model = GTK_TREE_MODEL(m_store);
        GtkTreeIter root_iter;
        if (gtk_tree_model_get_iter_first(model, &root_iter))
        {
            do
            {
                recompute_subtree(&root_iter);
            } while (gtk_tree_model_iter_next(model, &root_iter));
        }

        auto snapshot = compute_allocation_snapshot();
        double macro_effective_sum = 0.0;
        std::string violated_name;
        double violated_sum = 0.0;
        double violated_cap = 0.0;

        for (const auto& [name, explicit_val] : m_macro_explicit_cache)
        {
            double acct_sum = 0.0;
            if (auto sit = snapshot.summaries.find(name); sit != snapshot.summaries.end())
                acct_sum = sit->second.accounts_sum;
            if (explicit_val > 1e-6 && acct_sum > explicit_val + 0.01 && violated_name.empty())
            {
                violated_name = name;
                violated_sum = acct_sum;
                violated_cap = explicit_val;
            }
            macro_effective_sum += (explicit_val > 1e-6) ? explicit_val : acct_sum;
        }
        for (const auto& [name, summary] : snapshot.summaries)
        {
            if (m_macro_explicit_cache.find(name) == m_macro_explicit_cache.end())
                macro_effective_sum += summary.accounts_sum;
        }

        double total = snapshot.outside_macros_direct + macro_effective_sum;
        std::string total_str = format_allocation_number(total);
        gchar* markup = nullptr;
        if (!violated_name.empty())
        {
            std::string sum_str = format_allocation_number(violated_sum);
            std::string cap_str = format_allocation_number(violated_cap);
            gchar* esc = g_markup_escape_text(violated_name.c_str(), -1);
            markup = g_strdup_printf(
                "<span foreground='#c0392b'><b>%s \"%s\": %s%% &gt; %s%%</b></span>",
                _("Macro accounts sum exceeds target"), esc,
                sum_str.c_str(), cap_str.c_str());
            g_free(esc);
        }
        else if (std::fabs(total - 100.0) <= 0.01)
        {
            if (macro_effective_sum > 1e-6)
            {
                std::string acct_str = format_allocation_number(snapshot.outside_macros_direct);
                std::string macro_str = format_allocation_number(macro_effective_sum);
                markup = g_strdup_printf(
                    "<span foreground='#1b7a2e'><b>%s: %s%% ✓ (%s: %s%% + %s: %s%%)</b></span>",
                    _("Total Allocation"), total_str.c_str(),
                    _("Outside Macros"), acct_str.c_str(),
                    _("Macro Classes"), macro_str.c_str());
            }
            else
            {
                markup = g_strdup_printf(
                    "<span foreground='#1b7a2e'><b>%s: %s%% ✓</b></span>",
                    _("Total Allocation"), total_str.c_str());
            }
        }
        else if (total < 100.0)
        {
            std::string rem_str = format_allocation_number(100.0 - total);
            markup = g_strdup_printf(
                "<span foreground='#c0392b'><b>%s: %s%% (%s%% %s — %s)</b></span>",
                _("Total Allocation"), total_str.c_str(),
                rem_str.c_str(), _("remaining"), _("must equal 100%"));
        }
        else
        {
            std::string over_str = format_allocation_number(total - 100.0);
            markup = g_strdup_printf(
                "<span foreground='#c0392b'><b>%s: %s%% (%s%% %s — %s)</b></span>",
                _("Total Allocation"), total_str.c_str(),
                over_str.c_str(), _("over"), _("must equal 100%"));
        }
        gtk_label_set_markup(m_status_label, markup);
        g_free(markup);
    }

    void append_account_subtree(Account* acc, GtkTreeIter* parent_iter,
                                bool show_hidden)
    {
        if (!account_should_show(acc, show_hidden, m_allocations, m_account_macros))
            return;

        std::string guid = account_guid_str(acc);
        double direct = 0.0;
        if (auto it = m_allocations.find(guid); it != m_allocations.end())
            direct = it->second;

        std::string macro;
        if (auto mit = m_account_macros.find(guid); mit != m_account_macros.end())
            macro = mit->second;

        const char* name = xaccAccountGetName(acc);
        auto direct_text = format_pct_edit(direct);

        GtkTreeIter iter;
        gtk_tree_store_append(m_store, &iter, parent_iter);
        gtk_tree_store_set(m_store, &iter,
                           ALLOC_COL_GUID, guid.c_str(),
                           ALLOC_COL_NAME, name ? name : "",
                           ALLOC_COL_DIRECT_VAL, direct,
                           ALLOC_COL_DIRECT_TEXT, direct_text.c_str(),
                           ALLOC_COL_SUBTREE_VAL, 0.0,
                           ALLOC_COL_SUBTREE_TEXT, "",
                           ALLOC_COL_MACRO_NAME, macro.c_str(),
                           ALLOC_COL_MACRO_EFF_TEXT, "",
                           ALLOC_COL_WEIGHT, PANGO_WEIGHT_NORMAL,
                           -1);

        GList* children = gnc_account_get_children_sorted(acc);
        for (GList* n = children; n; n = n->next)
            append_account_subtree(static_cast<Account*>(n->data), &iter,
                                   show_hidden);
        g_list_free(children);
    }

    void populate_tree()
    {
        gtk_tree_store_clear(m_store);
        const bool show_hidden =
            m_show_hidden ? gtk_toggle_button_get_active(m_show_hidden) : false;
        auto root = gnc_get_current_root_account();
        if (root)
        {
            GList* top_accounts = gnc_account_get_children_sorted(root);
            for (GList* n = top_accounts; n; n = n->next)
                append_account_subtree(static_cast<Account*>(n->data),
                                       nullptr, show_hidden);
            g_list_free(top_accounts);
        }
        recompute_totals();
        refresh_macro_effective_cells();
        gtk_tree_view_expand_all(GTK_TREE_VIEW(get_widget()));
    }

    GtkTreeStore* m_store;
    GtkListStore* m_combo_store;
    GtkLabel* m_status_label;
    GtkToggleButton* m_show_hidden;
    GncOption* m_option;
    std::map<std::string, double> m_allocations;
    std::map<std::string, std::string> m_account_macros;
    std::map<std::string, double> m_macro_explicit_cache;
    std::map<std::string, double> m_macro_effective_cache;
    bool m_syncing{false};
};

GncGtkMacroClassesUIItem::GncGtkMacroClassesUIItem(GtkWidget* tree_view,
                                                   GtkListStore* store,
                                                   GtkLabel* status_label,
                                                   GtkTreeViewColumn* name_col,
                                                   GncOption* option) :
    GncOptionGtkUIItem{tree_view, GncOptionUIType::MACRO_CLASSES},
    m_store{store},
    m_status_label{status_label},
    m_name_col{name_col},
    m_option{option}
{
    s_macro_ui_items.push_back(this);
}

GncGtkMacroClassesUIItem::~GncGtkMacroClassesUIItem()
{
    s_macro_ui_items.erase(
        std::remove(s_macro_ui_items.begin(), s_macro_ui_items.end(), this),
        s_macro_ui_items.end());
}

void
GncGtkMacroClassesUIItem::set_ui_item_from_option(GncOption& option) noexcept
{
    m_macros = parse_macro_classes_string(option.get_value<std::string>());
    populate_list();
    notify_alloc_peer();
}

void
GncGtkMacroClassesUIItem::set_option_from_ui_item(GncOption& option) noexcept
{
    option.set_value(serialize_macro_classes(m_macros));
}

void
GncGtkMacroClassesUIItem::update_account_summaries(
    const std::map<std::string, MacroAccountSummary>& summaries,
    double outside_macros_direct)
{
    m_summaries = summaries;
    m_outside_macros_direct = outside_macros_direct;
    populate_list();
}

void
GncGtkMacroClassesUIItem::on_name_cell_edited(const gchar* path_str,
                                              const gchar* new_text,
                                              GncOption* option)
{
    auto path = gtk_tree_path_new_from_string(path_str);
    if (!path)
        return;
    int* indices = gtk_tree_path_get_indices(path);
    int idx = indices ? indices[0] : -1;
    gtk_tree_path_free(path);
    if (idx < 0 || static_cast<size_t>(idx) >= m_macros.size())
        return;

    std::string trimmed = new_text ? trim_ws(new_text) : std::string{};
    if (trimmed.empty())
        return;

    std::string old_name = m_macros[idx].first;
    if (trimmed == old_name)
        return;

    for (size_t i = 0; i < m_macros.size(); ++i)
    {
        if (i != static_cast<size_t>(idx) && m_macros[i].first == trimmed)
            return;
    }

    m_macros[idx].first = trimmed;
    if (auto sit = m_summaries.find(old_name); sit != m_summaries.end())
    {
        m_summaries[trimmed] = std::move(sit->second);
        m_summaries.erase(sit);
    }
    populate_list();
    notify_alloc_peer(old_name, trimmed);
    gnc_option_changed_widget_cb(get_widget(), option);
}

void
GncGtkMacroClassesUIItem::on_explicit_cell_edited(const gchar* path_str,
                                                  const gchar* new_text,
                                                  GncOption* option)
{
    auto path = gtk_tree_path_new_from_string(path_str);
    if (!path)
        return;
    int* indices = gtk_tree_path_get_indices(path);
    int idx = indices ? indices[0] : -1;
    gtk_tree_path_free(path);
    if (idx < 0 || static_cast<size_t>(idx) >= m_macros.size())
        return;

    double val = 0.0;
    if (!parse_user_percentage(new_text, val))
        return;

    m_macros[idx].second = val;
    populate_list();
    notify_alloc_peer();
    gnc_option_changed_widget_cb(get_widget(), option);
}

std::string
GncGtkMacroClassesUIItem::make_unique_macro_name() const
{
    const std::string base = _("New Macro Class");
    auto exists = [&](const std::string& candidate) {
        return std::any_of(m_macros.begin(), m_macros.end(),
                           [&](const auto& p) { return p.first == candidate; });
    };
    if (!exists(base))
        return base;
    for (int i = 2;; ++i)
    {
        std::string cand = base + " " + std::to_string(i);
        if (!exists(cand))
            return cand;
    }
}

void
GncGtkMacroClassesUIItem::on_add_clicked(GncOption* option)
{
    m_macros.emplace_back(make_unique_macro_name(), 0.0);
    populate_list();
    notify_alloc_peer();
    gnc_option_changed_widget_cb(get_widget(), option);

    auto path = gtk_tree_path_new_from_indices(
        static_cast<gint>(m_macros.size() - 1), -1);
    gtk_tree_view_set_cursor(GTK_TREE_VIEW(get_widget()), path, m_name_col, TRUE);
    gtk_tree_path_free(path);
}

void
GncGtkMacroClassesUIItem::on_remove_clicked(GncOption* option)
{
    auto selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(get_widget()));
    GtkTreeModel* model = nullptr;
    GtkTreeIter iter;
    if (!gtk_tree_selection_get_selected(selection, &model, &iter))
        return;
    auto path = gtk_tree_model_get_path(model, &iter);
    if (!path)
        return;
    int* indices = gtk_tree_path_get_indices(path);
    int idx = indices ? indices[0] : -1;
    gtk_tree_path_free(path);
    if (idx < 0 || static_cast<size_t>(idx) >= m_macros.size())
        return;

    m_summaries.erase(m_macros[idx].first);
    m_macros.erase(m_macros.begin() + idx);
    populate_list();
    notify_alloc_peer();
    gnc_option_changed_widget_cb(get_widget(), option);
}

void
GncGtkMacroClassesUIItem::on_clear_targets_clicked(GncOption* option)
{
    for (auto& [name, val] : m_macros)
        val = 0.0;
    populate_list();
    notify_alloc_peer();
    gnc_option_changed_widget_cb(get_widget(), option);
}

void
GncGtkMacroClassesUIItem::on_hierarchy_changed()
{
    notify_alloc_peer();
}

void
GncGtkMacroClassesUIItem::notify_alloc_peer(const std::string& renamed_from,
                                            const std::string& renamed_to)
{
    if (m_syncing)
        return;
    auto top = widget_toplevel_window(get_widget());
    if (!top)
        return;
    m_syncing = true;
    for (auto* peer : s_alloc_ui_items)
    {
        if (widget_toplevel_window(peer->get_widget()) == top)
            peer->sync_from_macro_peer(m_macros, renamed_from, renamed_to);
    }
    m_syncing = false;
}

void
GncGtkMacroClassesUIItem::update_status_label()
{
    double eff_sum = 0.0;
    std::string violated_name;
    double violated_sum = 0.0;
    double violated_cap = 0.0;
    for (const auto& [name, explicit_val] : m_macros)
    {
        double acct_sum = 0.0;
        if (auto it = m_summaries.find(name); it != m_summaries.end())
            acct_sum = it->second.accounts_sum;
        if (explicit_val > 1e-6 && acct_sum > explicit_val + 0.01 && violated_name.empty())
        {
            violated_name = name;
            violated_sum = acct_sum;
            violated_cap = explicit_val;
        }
        eff_sum += (explicit_val > 1e-6) ? explicit_val : acct_sum;
    }

    double grand_total = m_outside_macros_direct + eff_sum;
    std::string macro_str = format_allocation_number(eff_sum);
    std::string total_str = format_allocation_number(grand_total);
    gchar* markup = nullptr;
    if (!violated_name.empty())
    {
        std::string vsum_str = format_allocation_number(violated_sum);
        std::string vcap_str = format_allocation_number(violated_cap);
        gchar* esc = g_markup_escape_text(violated_name.c_str(), -1);
        markup = g_strdup_printf(
            "<span foreground='#c0392b'><b>%s: %zu  |  %s \"%s\": %s%% &gt; %s%%</b></span>",
            _("Macro Classes"), m_macros.size(),
            _("Accounts sum exceeds target in"), esc,
            vsum_str.c_str(), vcap_str.c_str());
        g_free(esc);
    }
    else if (std::fabs(grand_total - 100.0) <= 0.01)
    {
        markup = g_strdup_printf(
            "<span foreground='#1b7a2e'><b>%s: %zu (%s%%)  |  %s: %s%% ✓</b></span>",
            _("Macro Classes"), m_macros.size(), macro_str.c_str(),
            _("Total Allocation"), total_str.c_str());
    }
    else
    {
        markup = g_strdup_printf(
            "<span foreground='#c0392b'><b>%s: %zu (%s%%)  |  %s: %s%% (%s)</b></span>",
            _("Macro Classes"), m_macros.size(), macro_str.c_str(),
            _("Total Allocation"), total_str.c_str(), _("must equal 100%"));
    }
    gtk_label_set_markup(m_status_label, markup);
    g_free(markup);
}

void
GncGtkMacroClassesUIItem::populate_list()
{
    gtk_list_store_clear(m_store);
    for (const auto& [name, explicit_val] : m_macros)
    {
        double acct_sum = 0.0;
        std::string accts_desc = "—";
        if (auto it = m_summaries.find(name); it != m_summaries.end())
        {
            acct_sum = it->second.accounts_sum;
            const auto& names = it->second.account_names;
            if (!names.empty())
            {
                std::ostringstream oss;
                oss << names.size() << " (";
                size_t show_cnt = std::min<size_t>(names.size(), 4);
                for (size_t i = 0; i < show_cnt; ++i)
                {
                    if (i > 0)
                        oss << ", ";
                    oss << names[i];
                }
                if (names.size() > show_cnt)
                    oss << ", +" << (names.size() - show_cnt);
                oss << ")";
                accts_desc = oss.str();
            }
        }
        if (explicit_val > 1e-6 && acct_sum > explicit_val + 0.01)
            accts_desc += std::string{" ["} + _("EXCEEDS TARGET!") + "]";
        double effective = (explicit_val > 1e-6) ? explicit_val : acct_sum;
        auto explicit_text = format_pct_edit(explicit_val);
        auto sum_text = format_pct_display(acct_sum);
        auto eff_text = format_pct_display(effective);

        GtkTreeIter iter;
        gtk_list_store_append(m_store, &iter);
        gtk_list_store_set(m_store, &iter,
                           MACRO_COL_NAME, name.c_str(),
                           MACRO_COL_EXPLICIT_VAL, explicit_val,
                           MACRO_COL_EXPLICIT_TEXT, explicit_text.c_str(),
                           MACRO_COL_ACCTS_SUM_VAL, acct_sum,
                           MACRO_COL_ACCTS_SUM_TEXT, sum_text.c_str(),
                           MACRO_COL_EFFECTIVE_VAL, effective,
                           MACRO_COL_EFFECTIVE_TEXT, eff_text.c_str(),
                           MACRO_COL_ACCOUNTS_DESC, accts_desc.c_str(),
                           -1);
    }
    update_status_label();
}

static void
allocation_macro_cell_edited_cb(GtkCellRendererText* cell, gchar* path_str,
                                gchar* new_text, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkAccountAllocationUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_macro_cell_edited(path_str, new_text, option);
}

static void
allocation_hierarchy_changed_cb(GtkWidget* widget, GtkWidget* previous_toplevel,
                                gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkAccountAllocationUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_hierarchy_changed();
}

static void
macro_name_cell_edited_cb(GtkCellRendererText* cell, gchar* path_str,
                          gchar* new_text, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkMacroClassesUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_name_cell_edited(path_str, new_text, option);
}

static void
macro_explicit_cell_edited_cb(GtkCellRendererText* cell, gchar* path_str,
                              gchar* new_text, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkMacroClassesUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_explicit_cell_edited(path_str, new_text, option);
}

static void
macro_add_clicked_cb(GtkButton* button, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkMacroClassesUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_add_clicked(option);
}

static void
macro_remove_clicked_cb(GtkButton* button, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkMacroClassesUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_remove_clicked(option);
}

static void
macro_clear_targets_clicked_cb(GtkButton* button, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkMacroClassesUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_clear_targets_clicked(option);
}

static void
macro_hierarchy_changed_cb(GtkWidget* widget, GtkWidget* previous_toplevel,
                           gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkMacroClassesUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_hierarchy_changed();
}

static void
allocation_direct_cell_edited_cb(GtkCellRendererText* cell, gchar* path_str,
                                 gchar* new_text, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkAccountAllocationUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_direct_cell_edited(path_str, new_text, option);
}

static void
allocation_clear_all_cb(GtkButton* button, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkAccountAllocationUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_clear_all(option);
}

static void
allocation_expand_all_cb(GtkButton* button, gpointer data)
{
    auto tree_view = GTK_TREE_VIEW(data);
    if (tree_view)
        gtk_tree_view_expand_all(tree_view);
}

static void
allocation_collapse_all_cb(GtkButton* button, gpointer data)
{
    auto tree_view = GTK_TREE_VIEW(data);
    if (tree_view)
        gtk_tree_view_collapse_all(tree_view);
}

static void
allocation_show_hidden_toggled_cb(GtkToggleButton* button, gpointer data)
{
    auto option = static_cast<GncOption*>(data);
    if (!option)
        return;
    auto ui_item =
        dynamic_cast<GncGtkAccountAllocationUIItem*>(option->get_ui_item());
    if (ui_item)
        ui_item->on_show_hidden_toggled();
}

template<> void
create_option_widget<GncOptionUIType::ACCOUNT_ALLOCATION>(GncOption& option,
                                                          GtkGrid *page_box,
                                                          int row)
{
    auto store = gtk_tree_store_new(NUM_ALLOC_COLS,
                                    G_TYPE_STRING,
                                    G_TYPE_STRING,
                                    G_TYPE_DOUBLE,
                                    G_TYPE_STRING,
                                    G_TYPE_DOUBLE,
                                    G_TYPE_STRING,
                                    G_TYPE_STRING,
                                    G_TYPE_STRING,
                                    G_TYPE_INT);
    auto tree_view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
    g_object_unref(store);

    auto combo_store = gtk_list_store_new(1, G_TYPE_STRING);
    GtkTreeIter combo_iter;
    gtk_list_store_append(combo_store, &combo_iter);
    gtk_list_store_set(combo_store, &combo_iter, 0, _("(None)"), -1);

    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(tree_view), TRUE);
    gtk_tree_view_set_enable_tree_lines(GTK_TREE_VIEW(tree_view), TRUE);

    /* Column 0: Account hierarchy */
    auto acct_renderer = gtk_cell_renderer_text_new();
    auto acct_col = gtk_tree_view_column_new_with_attributes(
        _("Account"), acct_renderer,
        "text", ALLOC_COL_NAME,
        "weight", ALLOC_COL_WEIGHT,
        nullptr);
    gtk_tree_view_column_set_expand(acct_col, TRUE);
    gtk_tree_view_column_set_resizable(acct_col, TRUE);
    gtk_tree_view_column_set_min_width(acct_col, 280);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), acct_col);

    /* Column 1: Direct Target (%) editable cell */
    auto direct_renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(direct_renderer),
                 "editable", TRUE,
                 "xalign", 1.0,
                 nullptr);
    g_signal_connect(G_OBJECT(direct_renderer), "edited",
                     G_CALLBACK(allocation_direct_cell_edited_cb), &option);
    auto direct_col = gtk_tree_view_column_new_with_attributes(
        _("Direct Target (%)"), direct_renderer,
        "text", ALLOC_COL_DIRECT_TEXT,
        nullptr);
    gtk_tree_view_column_set_alignment(direct_col, 1.0);
    gtk_tree_view_column_set_min_width(direct_col, 120);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), direct_col);

    /* Column 2: Effective Subtree Total (%) read-only cell */
    auto subtree_renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(subtree_renderer), "xalign", 1.0, nullptr);
    auto subtree_col = gtk_tree_view_column_new_with_attributes(
        _("Effective Total (%)"), subtree_renderer,
        "text", ALLOC_COL_SUBTREE_TEXT,
        "weight", ALLOC_COL_WEIGHT,
        nullptr);
    gtk_tree_view_column_set_alignment(subtree_col, 1.0);
    gtk_tree_view_column_set_min_width(subtree_col, 125);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), subtree_col);

    /* Column 3: Macro Class dropdown selector */
    auto macro_renderer = gtk_cell_renderer_combo_new();
    g_object_set(G_OBJECT(macro_renderer),
                 "model", GTK_TREE_MODEL(combo_store),
                 "text-column", 0,
                 "has-entry", FALSE,
                 "editable", TRUE,
                 nullptr);
    g_signal_connect(G_OBJECT(macro_renderer), "edited",
                     G_CALLBACK(allocation_macro_cell_edited_cb), &option);
    auto macro_col = gtk_tree_view_column_new_with_attributes(
        _("Macro Class"), macro_renderer,
        "text", ALLOC_COL_MACRO_NAME,
        nullptr);
    gtk_tree_view_column_set_resizable(macro_col, TRUE);
    gtk_tree_view_column_set_min_width(macro_col, 160);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), macro_col);

    /* Column 4: Assigned Macro Class Effective Target (%) read-only cell */
    auto macro_eff_renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(macro_eff_renderer), "xalign", 1.0, nullptr);
    auto macro_eff_col = gtk_tree_view_column_new_with_attributes(
        _("Macro Effective (%)"), macro_eff_renderer,
        "text", ALLOC_COL_MACRO_EFF_TEXT,
        nullptr);
    gtk_tree_view_column_set_alignment(macro_eff_col, 1.0);
    gtk_tree_view_column_set_min_width(macro_eff_col, 125);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), macro_eff_col);

    auto frame = gtk_frame_new(_(option.get_name().c_str()));
    gtk_widget_set_hexpand(frame, TRUE);
    gtk_widget_set_vexpand(frame, TRUE);

    auto vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 6);
    gtk_container_add(GTK_CONTAINER(frame), vbox);

    auto help_label = gtk_label_new(
        _("Enter \"Direct Target (%)\" per account and optionally assign a \"Macro Class\" (virtual parent). Accounts inside a macro class must have direct targets summing <= the macro class target, and outside accounts + macro classes must sum to 100%."));
    gtk_label_set_line_wrap(GTK_LABEL(help_label), TRUE);
    gtk_widget_set_halign(help_label, GTK_ALIGN_START);
    gtk_label_set_xalign(GTK_LABEL(help_label), 0.0);
    gtk_box_pack_start(GTK_BOX(vbox), help_label, FALSE, FALSE, 0);

    auto scroll_win = gtk_scrolled_window_new(nullptr, nullptr);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll_win),
                                        GTK_SHADOW_IN);
    gtk_widget_set_size_request(scroll_win, 780, 420);
    gtk_widget_set_hexpand(scroll_win, TRUE);
    gtk_widget_set_vexpand(scroll_win, TRUE);
    gtk_container_add(GTK_CONTAINER(scroll_win), tree_view);
    gtk_box_pack_start(GTK_BOX(vbox), scroll_win, TRUE, TRUE, 0);

    auto status_label = gtk_label_new("");
    gtk_widget_set_halign(status_label, GTK_ALIGN_START);
    gtk_label_set_xalign(GTK_LABEL(status_label), 0.0);

    auto show_hidden_btn =
        gtk_check_button_new_with_label(_("Show Hidden Accounts"));
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(show_hidden_btn), FALSE);

    auto expand_btn = gtk_button_new_with_label(_("Expand All"));
    auto collapse_btn = gtk_button_new_with_label(_("Collapse All"));
    auto clear_btn = gtk_button_new_with_label(_("Clear All"));

    auto controls_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
    gtk_box_pack_start(GTK_BOX(controls_box), status_label, TRUE, TRUE, 0);
    gtk_box_pack_end(GTK_BOX(controls_box), clear_btn, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(controls_box), collapse_btn, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(controls_box), expand_btn, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(controls_box), show_hidden_btn, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), controls_box, FALSE, FALSE, 0);

    option.set_ui_item(std::make_unique<GncGtkAccountAllocationUIItem>(
        tree_view, store, combo_store, GTK_LABEL(status_label),
        GTK_TOGGLE_BUTTON(show_hidden_btn), &option));
    g_object_unref(combo_store);
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(tree_view), "hierarchy-changed",
                     G_CALLBACK(allocation_hierarchy_changed_cb), &option);
    g_signal_connect(G_OBJECT(show_hidden_btn), "toggled",
                     G_CALLBACK(allocation_show_hidden_toggled_cb), &option);
    g_signal_connect(G_OBJECT(expand_btn), "clicked",
                     G_CALLBACK(allocation_expand_all_cb), tree_view);
    g_signal_connect(G_OBJECT(collapse_btn), "clicked",
                     G_CALLBACK(allocation_collapse_all_cb), tree_view);
    g_signal_connect(G_OBJECT(clear_btn), "clicked",
                     G_CALLBACK(allocation_clear_all_cb), &option);

    set_tool_tip(option, frame);
    gtk_widget_set_halign(GTK_WIDGET(page_box), GTK_ALIGN_FILL);
    gtk_widget_set_hexpand(GTK_WIDGET(page_box), TRUE);
    gtk_widget_set_vexpand(GTK_WIDGET(page_box), TRUE);
    gtk_widget_show_all(frame);
    gtk_grid_attach(GTK_GRID(page_box), frame, 0, row, 2, 1);
}

template<> void
create_option_widget<GncOptionUIType::MACRO_CLASSES>(GncOption& option,
                                                     GtkGrid *page_box,
                                                     int row)
{
    auto store = gtk_list_store_new(NUM_MACRO_COLS,
                                    G_TYPE_STRING,
                                    G_TYPE_DOUBLE,
                                    G_TYPE_STRING,
                                    G_TYPE_DOUBLE,
                                    G_TYPE_STRING,
                                    G_TYPE_DOUBLE,
                                    G_TYPE_STRING,
                                    G_TYPE_STRING);
    auto tree_view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
    g_object_unref(store);

    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(tree_view), TRUE);

    /* Column 0: Macro Class name (editable) */
    auto name_renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(name_renderer), "editable", TRUE, nullptr);
    g_signal_connect(G_OBJECT(name_renderer), "edited",
                     G_CALLBACK(macro_name_cell_edited_cb), &option);
    auto name_col = gtk_tree_view_column_new_with_attributes(
        _("Macro Class"), name_renderer,
        "text", MACRO_COL_NAME,
        nullptr);
    gtk_tree_view_column_set_expand(name_col, TRUE);
    gtk_tree_view_column_set_resizable(name_col, TRUE);
    gtk_tree_view_column_set_min_width(name_col, 220);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), name_col);

    /* Column 1: Explicit Target (%) (editable; 0 or empty inherits Accounts Sum) */
    auto explicit_renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(explicit_renderer),
                 "editable", TRUE,
                 "xalign", 1.0,
                 nullptr);
    g_signal_connect(G_OBJECT(explicit_renderer), "edited",
                     G_CALLBACK(macro_explicit_cell_edited_cb), &option);
    auto explicit_col = gtk_tree_view_column_new_with_attributes(
        _("Target (%)"), explicit_renderer,
        "text", MACRO_COL_EXPLICIT_TEXT,
        nullptr);
    gtk_tree_view_column_set_alignment(explicit_col, 1.0);
    gtk_tree_view_column_set_min_width(explicit_col, 110);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), explicit_col);

    /* Column 2: Sum of Assigned Account Effective Targets (%) */
    auto sum_renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(sum_renderer), "xalign", 1.0, nullptr);
    auto sum_col = gtk_tree_view_column_new_with_attributes(
        _("Accounts Sum (%)"), sum_renderer,
        "text", MACRO_COL_ACCTS_SUM_TEXT,
        nullptr);
    gtk_tree_view_column_set_alignment(sum_col, 1.0);
    gtk_tree_view_column_set_min_width(sum_col, 130);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), sum_col);

    /* Column 3: Effective Target (%) = Target > 0 ? Target : Accounts Sum */
    auto eff_renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(eff_renderer),
                 "xalign", 1.0,
                 "weight", PANGO_WEIGHT_BOLD,
                 nullptr);
    auto eff_col = gtk_tree_view_column_new_with_attributes(
        _("Effective Target (%)"), eff_renderer,
        "text", MACRO_COL_EFFECTIVE_TEXT,
        nullptr);
    gtk_tree_view_column_set_alignment(eff_col, 1.0);
    gtk_tree_view_column_set_min_width(eff_col, 140);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), eff_col);

    /* Column 4: Assigned Accounts summary */
    auto accts_renderer = gtk_cell_renderer_text_new();
    auto accts_col = gtk_tree_view_column_new_with_attributes(
        _("Assigned Accounts"), accts_renderer,
        "text", MACRO_COL_ACCOUNTS_DESC,
        nullptr);
    gtk_tree_view_column_set_expand(accts_col, TRUE);
    gtk_tree_view_column_set_resizable(accts_col, TRUE);
    gtk_tree_view_column_set_min_width(accts_col, 200);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), accts_col);

    auto frame = gtk_frame_new(_(option.get_name().c_str()));
    gtk_widget_set_hexpand(frame, TRUE);
    gtk_widget_set_vexpand(frame, TRUE);

    auto vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 6);
    gtk_container_add(GTK_CONTAINER(frame), vbox);

    auto help_label = gtk_label_new(
        _("Define macro classes (virtual parents composing the 100% total) here, then assign accounts to them in the Allocation tab. The sum of account targets inside each macro class must be <= its Target (%), or leave Target empty/0 to inherit the sum."));
    gtk_label_set_line_wrap(GTK_LABEL(help_label), TRUE);
    gtk_widget_set_halign(help_label, GTK_ALIGN_START);
    gtk_label_set_xalign(GTK_LABEL(help_label), 0.0);
    gtk_box_pack_start(GTK_BOX(vbox), help_label, FALSE, FALSE, 0);

    auto scroll_win = gtk_scrolled_window_new(nullptr, nullptr);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll_win),
                                        GTK_SHADOW_IN);
    gtk_widget_set_size_request(scroll_win, 780, 400);
    gtk_widget_set_hexpand(scroll_win, TRUE);
    gtk_widget_set_vexpand(scroll_win, TRUE);
    gtk_container_add(GTK_CONTAINER(scroll_win), tree_view);
    gtk_box_pack_start(GTK_BOX(vbox), scroll_win, TRUE, TRUE, 0);

    auto status_label = gtk_label_new("");
    gtk_widget_set_halign(status_label, GTK_ALIGN_START);
    gtk_label_set_xalign(GTK_LABEL(status_label), 0.0);

    auto add_btn = gtk_button_new_with_label(_("Add Macro Class"));
    auto remove_btn = gtk_button_new_with_label(_("Remove Selected"));
    auto clear_btn = gtk_button_new_with_label(_("Clear Targets"));

    auto controls_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
    gtk_box_pack_start(GTK_BOX(controls_box), status_label, TRUE, TRUE, 0);
    gtk_box_pack_end(GTK_BOX(controls_box), clear_btn, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(controls_box), remove_btn, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(controls_box), add_btn, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), controls_box, FALSE, FALSE, 0);

    option.set_ui_item(std::make_unique<GncGtkMacroClassesUIItem>(
        tree_view, store, GTK_LABEL(status_label), name_col, &option));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(tree_view), "hierarchy-changed",
                     G_CALLBACK(macro_hierarchy_changed_cb), &option);
    g_signal_connect(G_OBJECT(add_btn), "clicked",
                     G_CALLBACK(macro_add_clicked_cb), &option);
    g_signal_connect(G_OBJECT(remove_btn), "clicked",
                     G_CALLBACK(macro_remove_clicked_cb), &option);
    g_signal_connect(G_OBJECT(clear_btn), "clicked",
                     G_CALLBACK(macro_clear_targets_clicked_cb), &option);

    set_tool_tip(option, frame);
    gtk_widget_set_halign(GTK_WIDGET(page_box), GTK_ALIGN_FILL);
    gtk_widget_set_hexpand(GTK_WIDGET(page_box), TRUE);
    gtk_widget_set_vexpand(GTK_WIDGET(page_box), TRUE);
    gtk_widget_show_all(frame);
    gtk_grid_attach(GTK_GRID(page_box), frame, 0, row, 2, 1);
}

static void
gnc_options_ui_factory_initialize(void)
{
    GncOptionUIFactory::set_func(GncOptionUIType::BOOLEAN,
                                 create_option_widget<GncOptionUIType::BOOLEAN>);
    GncOptionUIFactory::set_func(GncOptionUIType::STRING,
                                 create_option_widget<GncOptionUIType::STRING>);
    GncOptionUIFactory::set_func(GncOptionUIType::TEXT,
                                 create_option_widget<GncOptionUIType::TEXT>);
    GncOptionUIFactory::set_func(GncOptionUIType::CURRENCY,
                                 create_option_widget<GncOptionUIType::CURRENCY>);
    GncOptionUIFactory::set_func(GncOptionUIType::COMMODITY,
                                 create_option_widget<GncOptionUIType::COMMODITY>);
    GncOptionUIFactory::set_func(GncOptionUIType::MULTICHOICE,
                                 create_option_widget<GncOptionUIType::MULTICHOICE>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_ABSOLUTE,
                                 create_option_widget<GncOptionUIType::DATE_ABSOLUTE>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_RELATIVE,
                                 create_option_widget<GncOptionUIType::DATE_RELATIVE>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_BOTH,
                                 create_option_widget<GncOptionUIType::DATE_BOTH>);
    GncOptionUIFactory::set_func(GncOptionUIType::ACCOUNT_LIST,
                                 create_option_widget<GncOptionUIType::ACCOUNT_LIST>);
    GncOptionUIFactory::set_func(GncOptionUIType::ACCOUNT_SEL,
                                 create_option_widget<GncOptionUIType::ACCOUNT_SEL>);
    GncOptionUIFactory::set_func(GncOptionUIType::LIST,
                                 create_option_widget<GncOptionUIType::LIST>);
    GncOptionUIFactory::set_func(GncOptionUIType::NUMBER_RANGE,
                                 create_option_widget<GncOptionUIType::NUMBER_RANGE>);
    GncOptionUIFactory::set_func(GncOptionUIType::COLOR,
                                 create_option_widget<GncOptionUIType::COLOR>);
    GncOptionUIFactory::set_func(GncOptionUIType::FONT,
                                 create_option_widget<GncOptionUIType::FONT>);
    GncOptionUIFactory::set_func(GncOptionUIType::PLOT_SIZE,
                                 create_option_widget<GncOptionUIType::PLOT_SIZE>);
    GncOptionUIFactory::set_func(GncOptionUIType::BUDGET,
                                 create_option_widget<GncOptionUIType::BUDGET>);
    GncOptionUIFactory::set_func(GncOptionUIType::PIXMAP,
                                 create_option_widget<GncOptionUIType::PIXMAP>);
    GncOptionUIFactory::set_func(GncOptionUIType::RADIOBUTTON,
                                 create_option_widget<GncOptionUIType::RADIOBUTTON>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_FORMAT,
                                 create_option_widget<GncOptionUIType::DATE_FORMAT>);
    GncOptionUIFactory::set_func(GncOptionUIType::ACCOUNT_ALLOCATION,
                                 create_option_widget<GncOptionUIType::ACCOUNT_ALLOCATION>);
    GncOptionUIFactory::set_func(GncOptionUIType::MACRO_CLASSES,
                                 create_option_widget<GncOptionUIType::MACRO_CLASSES>);


}
