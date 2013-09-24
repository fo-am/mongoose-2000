// Copyright (C) 2013 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "sqlite/sqlite3.h"
#include "core/list.h"
#include <string.h>

using namespace std;

class db
{
public:
    db(const char *fn);
    ~db();

    list *exec(const char *sql);
    const char *status() { return m_status; }

    class value_node: public list::node
    {
    public:
        value_node(const char *v) { m_value=strdup(v); }
        ~value_node() { free(m_value); }
        char *m_value;
    };

    class row_node: public list::node
    {
    public:
        row_node(list *v) { m_row=v; }
        ~row_node() { delete m_row; }
        list *m_row;
    };

private:
    sqlite3 *m_db;

    int m_running;
    char m_status[4096];
};
