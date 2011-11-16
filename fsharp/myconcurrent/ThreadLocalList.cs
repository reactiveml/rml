using System;
using System.Threading;
using System.Diagnostics;

namespace System.Collections.Concurrent
{
    /// <summary>
    /// A class that represents a node in the lock thread list
    /// </summary>
    [Serializable]
    class Node<T>
    {
        public Node(T value)
        {
            m_value = value;
        }
        public T m_value;
        public Node<T> m_next;
        public Node<T> m_prev;
    }

    /// <summary>
    /// A class that represents the lock thread list
    /// </summary>
    class ThreadLocalList<T>
    {
        // Tead node in the list, null means the list is empty
        internal Node<T> m_head;

        // Tail node for the list
        private Node<T> m_tail;

        // The current list operation
        internal volatile int m_currentOp;

        // The list count from the Add/Take prespective
        private int m_count;

        // The stealing count
        internal int m_stealCount;

        // Next list in the dictionary values
        internal ThreadLocalList<T> m_nextList;

        // Set if the locl lock is taken
        internal bool m_lockTaken;

        // The owner thread for this list
        internal Thread m_ownerThread;

        // the version of the list, incremented only when the list changed from empty to non empty state
        internal volatile int m_version;

        /// <summary>
        /// ThreadLocalList constructor
        /// </summary>
        /// The owner thread for this list
        internal ThreadLocalList(Thread ownerThread)
        {
            m_ownerThread = ownerThread;
        }
        /// <summary>
        /// Add new item to head of the list
        /// </summary>
        /// The item to add.
        /// Whether to update the count.
        internal void Add(T item, bool updateCount)
        {
            checked
            {
                m_count++;
            }
            Node<T> node = new Node<T>(item);
            if (m_head == null)
            {
                Debug.Assert(m_tail == null);
                m_head = node;
                m_tail = node;
                m_version++; // changing from empty state to non empty state
            }
            else
            {
                node.m_next = m_head;
                m_head.m_prev = node;
                m_head = node;
            }
            if (updateCount) // update the count to avoid overflow if this add is synchronized
            {
                m_count = m_count - m_stealCount;
                m_stealCount = 0;
            }
        }

        /// <summary>
        /// Remove an item from the head of the list
        /// </summary>
        /// The removed item
        internal void Remove(out T result)
        {
            Debug.Assert(m_head != null);
            Node<T> head = m_head;
            m_head = m_head.m_next;
            if (m_head != null)
            {
                m_head.m_prev = null;
            }
            else
            {
                m_tail = null;
            }
            m_count--;
            result = head.m_value;

        }

        /// <summary>
        /// Peek an item from the head of the list
        /// </summary>
        /// the peeked item
        /// <returns>True if succeeded, false otherwise</returns>
        internal bool Peek(out T result)
        {
            Node<T> head = m_head;
            if (head != null)
            {
                result = head.m_value;
                return true;
            }
            result = default(T);
            return false;
        }

        /// <summary>
        /// Steal an item from the tail of the list
        /// </summary>
        /// the removed item
        /// remove or peek flag
        internal void Steal(out T result, bool remove)
        {
            Node<T> tail = m_tail;
            Debug.Assert(tail != null);
            if (remove) // Take operation
            {
                m_tail = m_tail.m_prev;
                if (m_tail != null)
                {
                    m_tail.m_next = null;
                }
                else
                {
                    m_head = null;
                }
                // Increment the steal count
                m_stealCount++;
            }
            result = tail.m_value;
        }


        internal void Flush()
        {
            m_count = 0;
            m_stealCount = 0;
            m_lockTaken = false;
            m_head = null;
            m_tail = null;
            m_currentOp = 0;
            m_version++;
        }


        internal void Prepend(ThreadLocalList<T> l)
        {
            if (l == null || l.m_head == null)
                return;

            if (m_head == null)
            {
                m_head = l.m_head;
                m_tail = l.m_tail;
                m_count = l.m_count;
                m_stealCount = l.m_stealCount;
            }
            else
            {
                l.m_tail.m_next = m_head;
                m_head.m_prev = l.m_tail;
                m_head = l.m_head;
                m_count = m_count + l.m_count;
                m_stealCount = m_stealCount + l.m_stealCount;
            }
        }

        /// <summary>
        /// Gets the total list count, it's not thread safe, may provide incorrect count if it is called concurrently
        /// </summary>
        internal int Count
        {
            get
            {
                return m_count - m_stealCount;
            }
        }
    }

    /// <summary>
    /// List operations
    /// </summary>
    enum ListOperation
    {
        None,
        Add,
        Take
    };

}