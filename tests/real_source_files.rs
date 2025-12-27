//! Integration tests that lex real source files from various languages.
//!
//! These tests verify the lexer can handle real-world code samples
//! from languages like Rust, Python, JavaScript, C, and more.
//!
//! Note: This lexer is a general-purpose tokenizer, not a language-specific
//! parser. Some language features may produce errors (e.g., Rust lifetimes
//! `'a` conflict with char literal syntax, Python `@` decorators are not
//! supported). Tests focus on what the lexer can handle well.

use lex::{DefaultLanguage, Lexer, TokenKind};

// ============================================================================
// Test Helpers
// ============================================================================

/// Asserts that lexing produces no errors
fn assert_no_errors(source: &str, errors: &[lex::LexError]) {
    if !errors.is_empty() {
        panic!(
            "Expected no lexer errors, but got {} errors:\n{:?}\n\nSource:\n{}",
            errors.len(),
            errors,
            source
        );
    }
}

/// Asserts that the lexer produces at least the expected number of tokens
fn assert_min_tokens(tokens: &[lex::Token], min: usize) {
    // Exclude EOF token from count
    let count = tokens.iter().filter(|t| t.kind != TokenKind::Eof).count();
    assert!(
        count >= min,
        "Expected at least {} tokens, but got {}",
        min,
        count
    );
}

/// Counts tokens of a specific kind
fn count_tokens(tokens: &[lex::Token], kind: TokenKind) -> usize {
    tokens.iter().filter(|t| t.kind == kind).count()
}

/// Gets keywords from tokens (returns the source text of keyword tokens)
fn get_keywords<'a>(tokens: &'a [lex::Token], source: &'a str) -> Vec<&'a str> {
    tokens
        .iter()
        .filter_map(|t| {
            if matches!(t.kind, TokenKind::Keyword(_)) {
                Some(&source[t.span.start..t.span.end])
            } else {
                None
            }
        })
        .collect()
}

// ============================================================================
// Rust Source Files
// ============================================================================

const RUST_FIBONACCI: &str = r#"
/// Calculates the nth Fibonacci number using dynamic programming.
pub fn fibonacci(n: u64) -> u64 {
    if n <= 1 {
        return n;
    }

    let mut prev = 0;
    let mut curr = 1;

    for _ in 2..=n {
        let next = prev + curr;
        prev = curr;
        curr = next;
    }

    curr
}

// Rust tests without attributes (attributes use # which is not a default operator)
mod tests {
    use super::*;

    fn test_fibonacci_base_cases() {
        assert_eq!(fibonacci(0), 0);
        assert_eq!(fibonacci(1), 1);
    }

    fn test_fibonacci_sequence() {
        let expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
        for (i, val) in expected.iter().enumerate() {
            assert_eq!(fibonacci(i as u64), *val);
        }
    }
}
"#;

const RUST_STRUCT_IMPL: &str = r#"
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

// Rust struct without derive attributes (attributes use # which is not a default operator)
pub struct Cache<K, V> {
    store: Arc<Mutex<HashMap<K, V>>>,
    max_size: usize,
}

impl<K, V> Cache<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    pub fn new(max_size: usize) -> Self {
        Self {
            store: Arc::new(Mutex::new(HashMap::new())),
            max_size,
        }
    }

    pub fn get(&self, key: &K) -> Option<V> {
        let guard = self.store.lock().unwrap();
        guard.get(key).cloned()
    }

    pub fn insert(&self, key: K, value: V) -> bool {
        let mut guard = self.store.lock().unwrap();
        if guard.len() >= self.max_size && !guard.contains_key(&key) {
            return false;
        }
        guard.insert(key, value);
        true
    }

    pub fn len(&self) -> usize {
        self.store.lock().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<K, V> Default for Cache<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    fn default() -> Self {
        Self::new(1000)
    }
}
"#;

const RUST_ASYNC: &str = r#"
use std::future::Future;

pub struct Timeout<F> {
    future: F,
    deadline: std::time::Instant,
}

pub struct TimeoutError;

pub async fn fetch_with_retry<T, E, F, Fut>(
    mut operation: F,
    max_retries: u32,
) -> Result<T, E>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<T, E>>,
{
    let mut attempts = 0;
    loop {
        match operation().await {
            Ok(result) => return Ok(result),
            Err(e) if attempts < max_retries => {
                attempts += 1;
                continue;
            }
            Err(e) => return Err(e),
        }
    }
}

async fn process_items<T>(items: Vec<T>) -> Vec<T> {
    let mut results = Vec::new();
    for item in items {
        results.push(item);
    }
    results
}
"#;

#[test]
fn test_rust_fibonacci() {
    let (tokens, errors) = Lexer::tokenize(RUST_FIBONACCI, DefaultLanguage);
    assert_no_errors(RUST_FIBONACCI, &errors);
    assert_min_tokens(&tokens, 100);

    // Should have function keywords
    let keywords = get_keywords(&tokens, RUST_FIBONACCI);
    assert!(keywords.contains(&"fn"));
    assert!(keywords.contains(&"pub"));
    assert!(keywords.contains(&"let"));
    assert!(keywords.contains(&"for"));
    assert!(keywords.contains(&"return"));
    assert!(keywords.contains(&"mod"));
    assert!(keywords.contains(&"use"));

    // Should have integer literals
    assert!(count_tokens(&tokens, TokenKind::IntLiteral) > 0);
}

#[test]
fn test_rust_struct_impl() {
    let (tokens, errors) = Lexer::tokenize(RUST_STRUCT_IMPL, DefaultLanguage);
    assert_no_errors(RUST_STRUCT_IMPL, &errors);
    assert_min_tokens(&tokens, 150);

    // Note: where is not in default keywords, will be an identifier
    let keywords = get_keywords(&tokens, RUST_STRUCT_IMPL);
    assert!(keywords.contains(&"struct"));
    assert!(keywords.contains(&"impl"));
    assert!(keywords.contains(&"pub"));
    assert!(keywords.contains(&"fn"));
    assert!(keywords.contains(&"Self"));
}

#[test]
fn test_rust_async() {
    let (tokens, errors) = Lexer::tokenize(RUST_ASYNC, DefaultLanguage);
    assert_no_errors(RUST_ASYNC, &errors);
    assert_min_tokens(&tokens, 80);

    // Note: async is not in default keywords, will be an identifier
    let keywords = get_keywords(&tokens, RUST_ASYNC);
    assert!(keywords.contains(&"fn"));
    assert!(keywords.contains(&"match"));
    assert!(keywords.contains(&"loop"));
}

// ============================================================================
// Python Source Files
// ============================================================================

const PYTHON_CLASS: &str = r#"
// Python-like code using C-style comments for lexer compatibility
import json
import os

class DataProcessor:
    """A class for processing and transforming data."""

    def __init__(self, config):
        self.config = config or {}
        self._cache = {}
        self.processed_count = 0

    def process(self, data):
        """Process a list of records."""
        results = []
        for record in data:
            if self._validate(record):
                transformed = self._transform(record)
                results.append(transformed)
                self.processed_count += 1
        return results

    def _validate(self, record):
        """Validate a single record."""
        required_fields = self.config.get("required_fields", [])
        return all(field in record for field in required_fields)

    def _transform(self, record):
        """Transform a single record."""
        result = record.copy()
        if "timestamp" in result:
            result["timestamp"] = str(result["timestamp"])
        return result

    def from_file(cls, path):
        """Create a processor from a config file."""
        with open(path, "r") as f:
            config = json.load(f)
        return cls(config)

    def merge_results(results):
        """Merge multiple result lists."""
        merged = []
        for result_list in results:
            merged.extend(result_list)
        return merged


def main():
    processor = DataProcessor({"required_fields": ["id", "name"]})

    data = [
        {"id": 1, "name": "Alice", "age": 30},
        {"id": 2, "name": "Bob", "age": 25},
        {"id": 3, "email": "test.example.com"},
    ]

    results = processor.process(data)
    print("Processed records")


if __name__ == "__main__":
    main()
"#;

const PYTHON_ASYNC: &str = r#"
// Async Python code using C-style comments for lexer compatibility
import asyncio

class Event:
    def __init__(self, id, name, data):
        self.id = id
        self.name = name
        self.data = data

async def fetch_events(url):
    """Fetch events from an API endpoint."""
    await asyncio.sleep(0.1)  // Simulate network delay
    return [
        Event(1, "start", {"user": "admin"}),
        Event(2, "process", {"items": 42}),
        Event(3, "end", {"status": "success"}),
    ]

async def process_event(event):
    """Process a single event asynchronously."""
    await asyncio.sleep(0.01)
    result = {
        "event_id": event.id,
        "processed": True,
        "result": "Handled event",
    }
    return result

async def process_all(events):
    """Process all events."""
    results = []
    for event in events:
        result = await process_event(event)
        results.append(result)
    return results

async def main():
    events = await fetch_events("https://api.example.com/events")

    // Process events concurrently
    tasks = [process_event(e) for e in events]
    results = await asyncio.gather(tasks)

    for result in results:
        print(result)

if __name__ == "__main__":
    asyncio.run(main())
"#;

#[test]
fn test_python_class() {
    // Use DefaultLanguage - Python keywords will be identifiers, but operators work
    // The goal is to test that the lexer handles real Python-like code
    let (tokens, errors) = Lexer::tokenize(PYTHON_CLASS, DefaultLanguage);
    assert_no_errors(PYTHON_CLASS, &errors);
    assert_min_tokens(&tokens, 150);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 50);
    assert!(count_tokens(&tokens, TokenKind::StringLiteral) > 5);
}

#[test]
fn test_python_async() {
    // Use DefaultLanguage - async/await are in default keywords
    let (tokens, errors) = Lexer::tokenize(PYTHON_ASYNC, DefaultLanguage);
    assert_no_errors(PYTHON_ASYNC, &errors);
    assert_min_tokens(&tokens, 100);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 30);
    assert!(count_tokens(&tokens, TokenKind::StringLiteral) > 3);
}

// ============================================================================
// JavaScript Source Files
// ============================================================================

const JAVASCRIPT_CLASS: &str = r#"
class EventEmitter {
    constructor() {
        this.listeners = new Map();
    }

    on(event, callback) {
        if (!this.listeners.has(event)) {
            this.listeners.set(event, []);
        }
        this.listeners.get(event).push(callback);
        return this;
    }

    off(event, callback) {
        if (!this.listeners.has(event)) return this;

        const callbacks = this.listeners.get(event);
        const index = callbacks.indexOf(callback);
        if (index !== -1) {
            callbacks.splice(index, 1);
        }
        return this;
    }

    emit(event, args) {
        if (!this.listeners.has(event)) return false;

        for (const callback of this.listeners.get(event)) {
            callback.apply(this, args);
        }
        return true;
    }

    once(event, callback) {
        const wrapper = (args) => {
            this.off(event, wrapper);
            callback.apply(this, args);
        };
        return this.on(event, wrapper);
    }
}

// Modern async/await patterns
async function fetchUserData(userId) {
    try {
        const response = await fetch("/api/users/" + userId);
        if (!response.ok) {
            throw new Error("HTTP error: " + response.status);
        }
        return await response.json();
    } catch (error) {
        console.error("Failed to fetch user:", error);
        return null;
    }
}

// Arrow functions
const processItems = (items) => {
    const results = [];
    for (const item of items) {
        results.push({
            id: item.id,
            processed: true
        });
    }
    return results;
};

// Object methods
const utils = {
    formatDate: function(date) {
        return date.toISOString();
    },
    parseValue: function(str) {
        return parseInt(str, 10);
    }
};

export { EventEmitter, fetchUserData, processItems, utils };
"#;

const JAVASCRIPT_REACT: &str = r#"
// React component without JSX (JSX would need special lexing)
// This demonstrates the JavaScript/React patterns the lexer can handle

import React from "react";

function createElement(type, props, children) {
    return {
        type: type,
        props: props || {},
        children: children || []
    };
}

function UserProfile(props) {
    const userId = props.userId;
    const onUpdate = props.onUpdate;

    const state = {
        user: null,
        loading: true,
        error: null
    };

    function setUser(data) {
        state.user = data;
    }

    function setLoading(value) {
        state.loading = value;
    }

    async function fetchUser() {
        try {
            setLoading(true);
            const response = await fetch("/api/users/" + userId);
            const data = await response.json();
            setUser(data);
            setLoading(false);
        } catch (err) {
            state.error = err.message;
            setLoading(false);
        }
    }

    function handleUpdate(field, value) {
        if (state.user) {
            state.user[field] = value;
        }
        if (onUpdate) {
            onUpdate({ field: field, value: value });
        }
    }

    function getDisplayName() {
        if (!state.user) return "";
        return state.user.firstName + " " + state.user.lastName;
    }

    // Return a representation of the component
    return {
        render: function() {
            if (state.loading) return "Loading...";
            if (state.error) return "Error: " + state.error;
            if (!state.user) return "User not found";
            return "User: " + getDisplayName();
        }
    };
}

export default UserProfile;
"#;

#[test]
fn test_javascript_class() {
    // Use DefaultLanguage - JS keywords overlap with default keywords
    let (tokens, errors) = Lexer::tokenize(JAVASCRIPT_CLASS, DefaultLanguage);
    assert_no_errors(JAVASCRIPT_CLASS, &errors);
    assert_min_tokens(&tokens, 150);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 50);
    assert!(count_tokens(&tokens, TokenKind::StringLiteral) > 2);

    // Some JS keywords are also default keywords
    let keywords = get_keywords(&tokens, JAVASCRIPT_CLASS);
    assert!(keywords.contains(&"if"));
    assert!(keywords.contains(&"for"));
    assert!(keywords.contains(&"return"));
}

#[test]
fn test_javascript_react() {
    // Use DefaultLanguage for operators
    let (tokens, errors) = Lexer::tokenize(JAVASCRIPT_REACT, DefaultLanguage);
    assert_no_errors(JAVASCRIPT_REACT, &errors);
    assert_min_tokens(&tokens, 100);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 30);
    assert!(count_tokens(&tokens, TokenKind::StringLiteral) > 3);
}

// ============================================================================
// C Source Files
// ============================================================================

const C_LINKED_LIST: &str = r#"
/* Linked list implementation in C (without preprocessor directives) */

typedef struct Node {
    int data;
    struct Node* next;
} Node;

typedef struct {
    Node* head;
    Node* tail;
    size_t length;
} LinkedList;

LinkedList* list_create(void) {
    LinkedList* list = (LinkedList*)malloc(sizeof(LinkedList));
    if (list == NULL) {
        return NULL;
    }
    list->head = NULL;
    list->tail = NULL;
    list->length = 0;
    return list;
}

void list_destroy(LinkedList* list) {
    if (list == NULL) return;

    Node* current = list->head;
    while (current != NULL) {
        Node* next = current->next;
        free(current);
        current = next;
    }
    free(list);
}

int list_push(LinkedList* list, int value) {
    if (list == NULL) return -1;

    Node* node = (Node*)malloc(sizeof(Node));
    if (node == NULL) return -1;

    node->data = value;
    node->next = NULL;

    if (list->tail == NULL) {
        list->head = node;
        list->tail = node;
    } else {
        list->tail->next = node;
        list->tail = node;
    }

    list->length++;
    return 0;
}

int list_pop(LinkedList* list, int* out_value) {
    if (list == NULL || list->head == NULL) return -1;

    if (list->head == list->tail) {
        *out_value = list->head->data;
        free(list->head);
        list->head = NULL;
        list->tail = NULL;
    } else {
        Node* current = list->head;
        while (current->next != list->tail) {
            current = current->next;
        }
        *out_value = list->tail->data;
        free(list->tail);
        list->tail = current;
        list->tail->next = NULL;
    }

    list->length--;
    return 0;
}

void list_print(const LinkedList* list) {
    if (list == NULL) {
        printf("(null)\n");
        return;
    }

    printf("[");
    Node* current = list->head;
    while (current != NULL) {
        printf("%d", current->data);
        if (current->next != NULL) {
            printf(", ");
        }
        current = current->next;
    }
    printf("]\n");
}

int main(void) {
    LinkedList* list = list_create();

    for (int i = 1; i <= 5; i++) {
        list_push(list, i * 10);
    }

    list_print(list);  /* Output: [10, 20, 30, 40, 50] */

    int value;
    while (list->length > 0) {
        list_pop(list, &value);
        printf("Popped: %d\n", value);
    }

    list_destroy(list);
    return 0;
}
"#;

const C_BUFFER: &str = r#"
/* Buffer implementation in C (without preprocessor directives) */

typedef enum {
    STATUS_OK = 0,
    STATUS_ERROR = -1,
    STATUS_NOMEM = -2,
    STATUS_INVALID = -3
} Status;

typedef struct {
    unsigned char* data;
    size_t size;
    size_t capacity;
} Buffer;

static inline Buffer* buffer_create(size_t initial_capacity) {
    Buffer* buf = (Buffer*)malloc(sizeof(Buffer));
    if (buf == NULL) return NULL;

    buf->data = (unsigned char*)malloc(initial_capacity);
    if (buf->data == NULL) {
        free(buf);
        return NULL;
    }

    buf->size = 0;
    buf->capacity = initial_capacity;
    return buf;
}

static inline void buffer_destroy(Buffer* buf) {
    if (buf != NULL) {
        free(buf->data);
        free(buf);
    }
}

static inline Status buffer_append(Buffer* buf, const unsigned char* data, size_t len) {
    if (buf == NULL || data == NULL) return STATUS_INVALID;

    if (buf->size + len > buf->capacity) {
        size_t new_capacity = buf->capacity * 2;
        if (new_capacity < buf->size + len) {
            new_capacity = buf->size + len;
        }
        unsigned char* new_data = (unsigned char*)realloc(buf->data, new_capacity);
        if (new_data == NULL) return STATUS_NOMEM;

        buf->data = new_data;
        buf->capacity = new_capacity;
    }

    memcpy(buf->data + buf->size, data, len);
    buf->size += len;
    return STATUS_OK;
}

static inline size_t buffer_size(const Buffer* buf) {
    if (buf == NULL) return 0;
    return buf->size;
}

static inline size_t buffer_capacity(const Buffer* buf) {
    if (buf == NULL) return 0;
    return buf->capacity;
}

static inline void buffer_clear(Buffer* buf) {
    if (buf != NULL) {
        buf->size = 0;
    }
}
"#;

#[test]
fn test_c_linked_list() {
    // Use DefaultLanguage - C shares many keywords with Rust-like default
    let (tokens, errors) = Lexer::tokenize(C_LINKED_LIST, DefaultLanguage);
    assert_no_errors(C_LINKED_LIST, &errors);
    assert_min_tokens(&tokens, 250);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 80);
    assert!(count_tokens(&tokens, TokenKind::IntLiteral) > 5);

    // Some C keywords are also default keywords
    // Note: C uses NULL (uppercase) but default language has null (lowercase)
    let keywords = get_keywords(&tokens, C_LINKED_LIST);
    assert!(keywords.contains(&"struct"));
    assert!(keywords.contains(&"if"));
    assert!(keywords.contains(&"for"));
    assert!(keywords.contains(&"while"));
    assert!(keywords.contains(&"return"));
}

#[test]
fn test_c_buffer() {
    // Use DefaultLanguage
    let (tokens, errors) = Lexer::tokenize(C_BUFFER, DefaultLanguage);
    assert_no_errors(C_BUFFER, &errors);
    assert_min_tokens(&tokens, 100);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 40);

    let keywords = get_keywords(&tokens, C_BUFFER);
    assert!(keywords.contains(&"struct"));
    assert!(keywords.contains(&"enum"));
    assert!(keywords.contains(&"if"));
    assert!(keywords.contains(&"return"));
}

// ============================================================================
// Go Source Files
// ============================================================================

const GO_HTTP_SERVER: &str = r#"
// Go HTTP server without backtick struct tags
// (backticks would need special handling)
package main

import (
    "encoding/json"
    "log"
    "net/http"
    "sync"
    "time"
)

type User struct {
    ID        int
    Name      string
    Email     string
    CreatedAt time.Time
}

type UserStore struct {
    mu    sync.RWMutex
    users map[int]*User
    nextID int
}

func NewUserStore() *UserStore {
    return &UserStore{
        users:  make(map[int]*User),
        nextID: 1,
    }
}

func (s *UserStore) Create(name, email string) *User {
    s.mu.Lock()
    defer s.mu.Unlock()

    user := &User{
        ID:        s.nextID,
        Name:      name,
        Email:     email,
        CreatedAt: time.Now(),
    }
    s.users[user.ID] = user
    s.nextID++
    return user
}

func (s *UserStore) Get(id int) (*User, bool) {
    s.mu.RLock()
    defer s.mu.RUnlock()
    user, ok := s.users[id]
    return user, ok
}

func (s *UserStore) List() []*User {
    s.mu.RLock()
    defer s.mu.RUnlock()

    result := make([]*User, 0, len(s.users))
    for _, user := range s.users {
        result = append(result, user)
    }
    return result
}

type Server struct {
    store  *UserStore
    router *http.ServeMux
}

func NewServer() *Server {
    s := &Server{
        store:  NewUserStore(),
        router: http.NewServeMux(),
    }
    s.routes()
    return s
}

func (s *Server) routes() {
    s.router.HandleFunc("/users", s.handleListUsers)
    s.router.HandleFunc("/users/create", s.handleCreateUser)
}

func (s *Server) handleListUsers(w http.ResponseWriter, r *http.Request) {
    users := s.store.List()
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(users)
}

func (s *Server) handleCreateUser(w http.ResponseWriter, r *http.Request) {
    var input struct {
        Name  string
        Email string
    }

    if err := json.NewDecoder(r.Body).Decode(&input); err != nil {
        http.Error(w, "Invalid request body", http.StatusBadRequest)
        return
    }

    user := s.store.Create(input.Name, input.Email)
    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(http.StatusCreated)
    json.NewEncoder(w).Encode(user)
}

func main() {
    server := NewServer()

    // Add some test data
    server.store.Create("Alice", "alice@example.com")
    server.store.Create("Bob", "bob@example.com")

    log.Println("Starting server on :8080")
    if err := http.ListenAndServe(":8080", server.router); err != nil {
        log.Fatal(err)
    }
}
"#;

#[test]
fn test_go_http_server() {
    // Use DefaultLanguage - Go has similar syntax to C
    let (tokens, errors) = Lexer::tokenize(GO_HTTP_SERVER, DefaultLanguage);
    assert_no_errors(GO_HTTP_SERVER, &errors);
    assert_min_tokens(&tokens, 250);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 80);
    assert!(count_tokens(&tokens, TokenKind::StringLiteral) > 5);

    // Some Go keywords are also default keywords
    let keywords = get_keywords(&tokens, GO_HTTP_SERVER);
    assert!(keywords.contains(&"type"));
    assert!(keywords.contains(&"struct"));
    assert!(keywords.contains(&"return"));
    assert!(keywords.contains(&"if"));
    assert!(keywords.contains(&"for"));
}

// ============================================================================
// TypeScript Source Files
// ============================================================================

const TYPESCRIPT_GENERICS: &str = r#"
// TypeScript generics without ?? operator (not in default operators)
interface Comparable<T> {
    compareTo(other: T): number;
}

interface Sortable<T extends Comparable<T>> {
    sort(): T[];
    sortDescending(): T[];
}

class SortedList<T extends Comparable<T>> implements Sortable<T> {
    private items: T[] = [];

    constructor(initialItems: T[]) {
        if (initialItems) {
            this.items = initialItems.slice();
        }
    }

    add(item: T): void {
        this.items.push(item);
    }

    remove(item: T): boolean {
        const index = this.items.findIndex(function(i) {
            return i.compareTo(item) === 0;
        });
        if (index !== -1) {
            this.items.splice(index, 1);
            return true;
        }
        return false;
    }

    sort(): T[] {
        return this.items.slice().sort(function(a, b) {
            return a.compareTo(b);
        });
    }

    sortDescending(): T[] {
        return this.items.slice().sort(function(a, b) {
            return b.compareTo(a);
        });
    }

    get length(): number {
        return this.items.length;
    }
}

// Utility types
type Result<T, E> = { success: boolean; value: T; error: E };

// Generic function with constraints
function groupBy<T, K>(items: T[], keyFn: (item: T) => K): Map<K, T[]> {
    const result = new Map<K, T[]>();

    for (const item of items) {
        const key = keyFn(item);
        let existing = result.get(key);
        if (!existing) {
            existing = [];
        }
        existing.push(item);
        result.set(key, existing);
    }

    return result;
}

// Simple type aliases
type StringMap<T> = Map<string, T>;
type NumberArray = number[];

export { SortedList, groupBy, Result, StringMap, NumberArray };
"#;

#[test]
fn test_typescript_generics() {
    // Use DefaultLanguage - TS has JavaScript-like syntax
    let (tokens, errors) = Lexer::tokenize(TYPESCRIPT_GENERICS, DefaultLanguage);
    assert_no_errors(TYPESCRIPT_GENERICS, &errors);
    assert_min_tokens(&tokens, 150);

    // Verify we have reasonable token structure
    assert!(count_tokens(&tokens, TokenKind::Ident) > 50);

    // Some TS keywords are also default keywords
    let keywords = get_keywords(&tokens, TYPESCRIPT_GENERICS);
    assert!(keywords.contains(&"type"));
    assert!(keywords.contains(&"return"));
    assert!(keywords.contains(&"for"));
    assert!(keywords.contains(&"if"));
}

// ============================================================================
// Mixed Language Edge Cases
// ============================================================================

const RUST_COMPLEX_GENERICS: &str = r#"
// Complex generics without lifetime annotations
// (lifetimes like 'a conflict with char literal syntax)
use std::marker::PhantomData;

pub trait Parser<Output> {
    fn parse(&self, input: String) -> Result<(String, Output), String>;

    fn map<F, NewOutput>(self, map_fn: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Output) -> NewOutput,
    {
        Map {
            parser: self,
            map_fn,
        }
    }

    fn and_then<F, NextParser, NextOutput>(self, f: F) -> AndThen<Self, F>
    where
        Self: Sized,
        F: Fn(Output) -> NextParser,
        NextParser: Parser<NextOutput>,
    {
        AndThen {
            parser: self,
            f,
            _phantom: PhantomData,
        }
    }
}

pub struct Map<P, F> {
    parser: P,
    map_fn: F,
}

impl<P, F, Output, NewOutput> Parser<NewOutput> for Map<P, F>
where
    P: Parser<Output>,
    F: Fn(Output) -> NewOutput,
{
    fn parse(&self, input: String) -> Result<(String, NewOutput), String> {
        self.parser
            .parse(input)
            .map(|(rest, output)| (rest, (self.map_fn)(output)))
    }
}

pub struct AndThen<P, F> {
    parser: P,
    f: F,
    _phantom: PhantomData<()>,
}

impl<P, F, NextParser, Output, NextOutput> Parser<NextOutput> for AndThen<P, F>
where
    P: Parser<Output>,
    F: Fn(Output) -> NextParser,
    NextParser: Parser<NextOutput>,
{
    fn parse(&self, input: String) -> Result<(String, NextOutput), String> {
        match self.parser.parse(input) {
            Ok((rest, output)) => (self.f)(output).parse(rest),
            Err(e) => Err(e),
        }
    }
}
"#;

#[test]
fn test_rust_complex_generics() {
    let (tokens, errors) = Lexer::tokenize(RUST_COMPLEX_GENERICS, DefaultLanguage);
    assert_no_errors(RUST_COMPLEX_GENERICS, &errors);
    assert_min_tokens(&tokens, 150);

    // Note: where is not in default keywords, will be an identifier
    let keywords = get_keywords(&tokens, RUST_COMPLEX_GENERICS);
    assert!(keywords.contains(&"pub"));
    assert!(keywords.contains(&"trait"));
    assert!(keywords.contains(&"fn"));
    assert!(keywords.contains(&"Self"));
    assert!(keywords.contains(&"impl"));
    assert!(keywords.contains(&"struct"));
}

// ============================================================================
// Code with Unicode
// ============================================================================

const RUST_UNICODE: &str = r#"
// Japanese variable names
let 変数 = 42;
let データ = "こんにちは";

// Chinese function
fn 計算(數字: i32) -> i32 {
    數字 * 2
}

// Greek letters (common in math)
let α = 3.14159;
let β = 2.71828;
let Δx = α - β;

// Emoji in strings
let greeting = "Hello 👋 World 🌍!";
let flags = "🇺🇸 🇯🇵 🇬🇧";

// Cyrillic
let привет = "мир";

// Arabic
let مرحبا = "عالم";

// Mixed
fn mixed_計算(α: f64, β: f64) -> f64 {
    α + β
}
"#;

#[test]
fn test_rust_unicode() {
    let (tokens, errors) = Lexer::tokenize(RUST_UNICODE, DefaultLanguage);
    assert_no_errors(RUST_UNICODE, &errors);
    assert_min_tokens(&tokens, 40);

    // Should recognize Unicode identifiers
    assert!(count_tokens(&tokens, TokenKind::Ident) > 10);

    // Should recognize string literals with emoji
    assert!(count_tokens(&tokens, TokenKind::StringLiteral) > 3);
}

// ============================================================================
// Large File Stress Test
// ============================================================================

/// Generate a large Rust-like source file for stress testing
fn generate_large_source(num_functions: usize) -> String {
    let mut source = String::new();
    source.push_str("// Auto-generated stress test file\n\n");

    for i in 0..num_functions {
        source.push_str(&format!(
            r#"
/// Function number {i}
pub fn function_{i}(arg_{i}: i32) -> i32 {{
    let local_{i} = arg_{i} * 2;
    let result_{i} = if local_{i} > 100 {{
        local_{i} - 50
    }} else {{
        local_{i} + 50
    }};
    result_{i}
}}
"#
        ));
    }

    source
}

#[test]
fn test_large_file() {
    let source = generate_large_source(100);
    let (tokens, errors) = Lexer::tokenize(&source, DefaultLanguage);

    assert_no_errors(&source, &errors);
    // Each function should produce roughly 30-40 tokens
    assert_min_tokens(&tokens, 2500);

    // Verify we got the expected keywords
    let keywords = get_keywords(&tokens, &source);
    assert!(keywords.iter().filter(|&&k| k == "pub").count() >= 100);
    assert!(keywords.iter().filter(|&&k| k == "fn").count() >= 100);
    assert!(keywords.iter().filter(|&&k| k == "let").count() >= 200);
}

#[test]
fn test_very_large_file() {
    let source = generate_large_source(500);
    let (tokens, errors) = Lexer::tokenize(&source, DefaultLanguage);

    assert_no_errors(&source, &errors);
    assert_min_tokens(&tokens, 12500);
}

// ============================================================================
// Real-world Edge Cases
// ============================================================================

#[test]
fn test_empty_source() {
    let (tokens, errors) = Lexer::tokenize("", DefaultLanguage);
    assert_no_errors("", &errors);
    // Iterator doesn't include EOF, so empty source produces 0 tokens
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_whitespace_only() {
    let source = "   \n\t\n   \r\n   ";
    let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
    assert_no_errors(source, &errors);
    // Whitespace is skipped by default, iterator doesn't include EOF
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_comments_only() {
    let source = r#"
// This is a comment
/* This is a
   multi-line
   comment */
// Another comment
"#;
    let (_tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
    assert_no_errors(source, &errors);
}

#[test]
fn test_nested_braces() {
    let source = r#"
fn deeply_nested() {
    {
        {
            {
                {
                    let x = 1;
                }
            }
        }
    }
}
"#;
    let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
    assert_no_errors(source, &errors);

    let open_braces = count_tokens(&tokens, TokenKind::LBrace);
    let close_braces = count_tokens(&tokens, TokenKind::RBrace);
    assert_eq!(open_braces, close_braces);
    assert_eq!(open_braces, 5);
}

#[test]
fn test_all_operators() {
    let source = r#"
let a = 1 + 2 - 3 * 4 / 5 % 6;
let b = a == b != c < d > e <= f >= g;
let c = !a && b || c;
let d = a & b | c ^ d ~ e;
let e = a << b >> c;
let f = a -> b => c;
let g = a::b.c;
"#;
    let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
    assert_no_errors(source, &errors);

    // Check various operators exist
    assert!(count_tokens(&tokens, TokenKind::Plus) > 0);
    assert!(count_tokens(&tokens, TokenKind::Minus) > 0);
    assert!(count_tokens(&tokens, TokenKind::Star) > 0);
    assert!(count_tokens(&tokens, TokenKind::Slash) > 0);
    assert!(count_tokens(&tokens, TokenKind::EqEq) > 0);
    assert!(count_tokens(&tokens, TokenKind::BangEq) > 0);
    assert!(count_tokens(&tokens, TokenKind::AmpAmp) > 0);
    assert!(count_tokens(&tokens, TokenKind::PipePipe) > 0);
}

#[test]
fn test_all_number_formats() {
    let source = r#"
let decimal = 42;
let decimal_underscores = 1_000_000;
let hex = 0xFF;
let hex_upper = 0XFF;
let octal = 0o77;
let binary = 0b1010;
let float = 3.14159;
let float_exp = 1.0e10;
let float_exp_neg = 1.0e-10;
let float_exp_upper = 1.0E10;
"#;
    let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
    assert_no_errors(source, &errors);

    let int_count = count_tokens(&tokens, TokenKind::IntLiteral);
    let float_count = count_tokens(&tokens, TokenKind::FloatLiteral);

    // Should have 6 int literals and 4 float literals
    assert_eq!(int_count, 6);
    assert_eq!(float_count, 4);
}
