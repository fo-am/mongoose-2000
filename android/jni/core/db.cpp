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

#include "db.h"
#include <stdio.h>

db::db(const char *fn) :
    m_running(1)
{
    snprintf(m_status,2048,"Starting up");
    int rc = sqlite3_open(fn, &m_db);
    if(rc)
    {
        snprintf(m_status,2048,"Can't open database: %s",sqlite3_errmsg(m_db));
        m_running=0;
    }
    else
    {
        snprintf(m_status,2048,"Opened database successfully");
    }
}

db::~db()
{
}

static int callback(void *d, int argc, char **argv, char **azColName){
   int i;
   list *data=(list*)d;
   list *row = new list;
   for(i=0; i<argc; i++)
   {
       row->add_to_end(new db::value_node(argv[i] ? argv[i] : "NULL"));
   }
   data->add_to_end(new db::row_node(row));
   return 0;
}

/*
void db::print_data(list *d)
{
    row_node *row=(row_node*)d->m_head;
    while (row!=NULL)
    {
        value_node *value=(value_node*)row->m_row->m_head;
        while (value!=NULL)
        {
            cerr<<value->m_value<<" ";
            value=(value_node*)value->m_next;
        }

        row=(row_node*)row->m_next;
        cerr<<endl;
    }
}
*/

list *db::exec(const char *sql)
{
    if (!m_running) return NULL;

    char *err = 0;
    list *data = new list;
    int rc = sqlite3_exec(m_db, sql, callback, data, &err);

    if( rc != SQLITE_OK )
    {
        snprintf(m_status,4096,"SQL error: %s",err);
        //m_running=0;
        sqlite3_free(err);
    }
    else
    {
        snprintf(m_status,4096,"SQL GOOD.");
    }

    return data;
}
