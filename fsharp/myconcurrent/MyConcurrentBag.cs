   // ==++==
//
//   Copyright (c) Microsoft Corporation.  All rights reserved.
//
// ==--==
// =+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
//
// ConcurrentBag.cs
//
// <owner>[....]</owner>
//
//
//An unordered collection that allows duplicates and that provides add and get operations.
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Runtime.Serialization;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.Security.Permissions;
using System.Threading;
using System.Diagnostics.CodeAnalysis;
  
namespace System.Collections.Concurrent
{
    /// <summary>
    /// Represents an thread-safe, unordered collection of objects.
    /// </summary>
    /// <typeparam name="T">Specifies the type of elements in the bag.</typeparam>
    /// <remarks>
    /// <para>
    /// Bags are useful for storing objects when ordering doesn't matter, and unlike sets, bags support
    /// duplicates. <see cref="ConcurrentBag{T}"> is a thread-safe bag implementation, optimized for
    /// scenarios where the same thread will be both producing and consuming data stored in the bag.
    /// </see></para>
    /// <para>
    /// <see cref="ConcurrentBag{T}"> accepts null reference (Nothing in Visual Basic) as a valid
    /// value for reference types.
    /// </see></para>
    /// <para>
    /// All public and protected members of <see cref="ConcurrentBag{T}"> are thread-safe and may be used
    /// concurrently from multiple threads.
    /// </see></para>
    /// </remarks>
    [Serializable]
    [ComVisible(false)]
    [DebuggerDisplay("Count = {Count}")]
    [HostProtection(Synchronization = true, ExternalThreading = true)]
    public class MyConcurrentBag<T> : IProducerConsumerCollection<T>
    {
  
        // ThreadLocalList object that contains the data per thread
        [NonSerialized]
        ThreadLocal<ThreadLocalList<T>> m_locals;
  
        // This head and tail pointers points to the first and last local lists, to allow enumeration on the thread locals objects
        [NonSerialized]
        volatile ThreadLocalList<T> m_headList, m_tailList;
 
        // A global lock object, used in two cases:
        // 1- To  maintain the m_tailList pointer for each new list addition process ( first time a thread called Add )
        // 2- To freeze the bag in GetEnumerator, CopyTo, ToArray and Count members
        [NonSerialized]
        object m_globalListsLock;
 
        // A flag used to tell the operations thread that it must synchronize the operation, this flag is set/unset within
        // m_globalListsLock lock
        [NonSerialized]
        bool m_needSync;
 
        // Used for custom serialization.
        private T[] m_serializationArray;
  
        /// <summary>
        /// Initializes a new instance of the <see cref="ConcurrentBag{T}">
        /// class.
        /// </see></summary>
        public MyConcurrentBag()
        {
            Initialize(null);
        }
  
        /// <summary>
        /// Initializes a new instance of the <see cref="ConcurrentBag{T}">
        /// class that contains elements copied from the specified collection.
        /// </see></summary>
        /// The collection whose elements are copied to the new <see cref="ConcurrentBag{T}">.
        /// <exception cref="ArgumentNullException"><paramref name="collection"> is a null reference
        /// (Nothing in Visual Basic).</paramref></exception>
        public MyConcurrentBag(IEnumerable<T> collection)
        {
            if (collection == null)
            {
                throw new ArgumentNullException("collection", "SR.ConcurrentBag_Ctor_ArgumentNullException");
            }
            Initialize(collection);
        }
 
  
        /// <summary>
        /// Local helper function to initalize a new bag object
        /// </summary>
        /// An enumeration containing items with which to initialize this bag.
        private void Initialize(IEnumerable<T> collection)
        {
            m_locals = new ThreadLocal<ThreadLocalList<T>>();
            m_globalListsLock = new object();
  
            // Copy the collection to the bag
            if (collection != null)
            {
                ThreadLocalList<T> list = GetThreadList(true);
                foreach (T item in collection)
                {
                    AddInternal(list, item);
                }
            }
        }
  
        /// <summary>
        /// Adds an object to the <see cref="ConcurrentBag{T}">.
        /// </see></summary>
        /// The object to be added to the
        /// <see cref="ConcurrentBag{T}">. The value can be a null reference
        /// (Nothing in Visual Basic) for reference types.
        public void Add(T item)
        {
            // Get the local list for that thread, create a new list if this thread doesn't exist
            //(first time to call add)
            ThreadLocalList<T> list = GetThreadList(true);
            AddInternal(list, item);
        }


        public void AddCollection(IEnumerable<T> items, bool iunsafe = false)
        {
            bool lockTaken = false;
            try
            {
                if (!iunsafe)  
                  FreezeBag(ref lockTaken);
                ThreadLocalList<T> current_list = m_headList;
                foreach (T item in items)
                {
                    current_list.Add(item, true);
                    current_list = current_list.m_nextList;
                    if (current_list == null)
                        current_list = m_headList;
                }
            }
            finally 
            {         
                if (!iunsafe)   
                    UnfreezeBag(lockTaken); 
            } 
        }

        public void AppendBag(MyConcurrentBag<T> bag)
        {
            ThreadLocalList<T> currentList = m_headList;
            ThreadLocalList<T> bagCurrentList = bag.m_headList;

            int totalCount = bag.Count;

            if (currentList == null)
            {
                AddCollection(bag);
                return;
            }

            //Console.WriteLine("Curent count");
            //while (currentList != null)
            //{
            //    Console.WriteLine (currentList.Count);
            //    currentList = currentList.m_nextList;
            //}
            //currentList = m_headList;
            //Console.WriteLine(" ");

            while (bagCurrentList != null)
            {
                //Console.WriteLine("Total: " + totalCount + "; add: " + bagCurrentList.Count);
                if (bagCurrentList.Count > (2 * totalCount / 3))
                {
                    //Console.WriteLine("Bobo");
                    Node<T> currentNode = bagCurrentList.m_head;
                    while (currentNode != null)
                    {
                        currentList.Add(currentNode.m_value, false);
                        currentNode = currentNode.m_next;
                        currentList = currentList.m_nextList;
                        if (currentList == null)
                            currentList = m_headList;
                    }
                }
                else
                {
                    currentList.Prepend(bagCurrentList);

                    currentList = currentList.m_nextList;
                    if (currentList == null)
                        currentList = m_headList;
                }

                bagCurrentList.Flush();
                bagCurrentList = bagCurrentList.m_nextList;

            }
        }


        /// <summary>
        /// </summary>
        ///
        /// 
        private void AddInternal(ThreadLocalList<T> list, T item)
        {
            bool lockTaken = false;
            try
            {
#pragma warning disable 0420
                Interlocked.Exchange(ref list.m_currentOp, (int)ListOperation.Add);
#pragma warning restore 0420
                //Synchronization cases:
                // if the list count is less than two to avoid conflict with any stealing thread
                // if m_needSync is set, this means there is a thread that needs to freeze the bag
                if (list.Count < 2 || m_needSync)
                {
                    // reset it back to zero to avoid deadlock with stealing thread
                    list.m_currentOp = (int)ListOperation.None;
                    Monitor.Enter(list, ref lockTaken);
                }
                list.Add(item, lockTaken);
            }
            finally
            {
                list.m_currentOp = (int)ListOperation.None;
                if (lockTaken)
                {
                    Monitor.Exit(list);
                }
            }
        }
 
        /// <summary>
        /// Attempts to add an object to the <see cref="ConcurrentBag{T}">.
        /// </see></summary>
        /// The object to be added to the
        /// <see cref="ConcurrentBag{T}">. The value can be a null reference
        /// (Nothing in Visual Basic) for reference types.
        /// <returns>Always returns true</returns>
        bool IProducerConsumerCollection<T>.TryAdd(T item)
        {
            Add(item);
            return true;
        }
 
        /// <summary>
        /// Attempts to remove and return an object from the <see cref="ConcurrentBag{T}">.
        /// </see></summary>
        /// When this method returns, <paramref name="result"> contains the object
        /// removed from the <see cref="ConcurrentBag{T}"> or the default value
        /// of <typeparamref name="T"> if the operation failed.
        /// <returns>true if an object was removed successfully; otherwise, false.</returns>
        public bool TryTake(out T result)
        {
            return TryTakeOrPeek(out result, true);
        }
  
        /// <summary>
        /// Attempts to return an object from the <see cref="ConcurrentBag{T}">
        /// without removing it.
        /// </see></summary>
        /// When this method returns, <paramref name="result"> contains an object from
        /// the <see cref="ConcurrentBag{T}"> or the default value of
        /// <typeparamref name="T"> if the operation failed.
        /// <returns>true if and object was returned successfully; otherwise, false.</returns>
        public bool TryPeek(out T result)
        {
            return TryTakeOrPeek(out result, false);
        }
 
        /// <summary>
        /// Local helper function to Take or Peek an item from the bag
        /// </summary>
        /// To receive the item retrieved from the bag
        /// True means Take operation, false means Peek operation
        /// <returns>True if succeeded, false otherwise</returns>
        private bool TryTakeOrPeek(out T result, bool take)
        {
 
            // Get the local list for that thread, return null if the thread doesn't exit
            //(this thread never add before)
            ThreadLocalList<T> list = GetThreadList(true);
            if (list == null || list.Count == 0)
            {
                return Steal(out result, take);
            }
 
            bool lockTaken = false;
            try
            {
                if (take) // Take operation
                {
#pragma warning disable 0420
                    Interlocked.Exchange(ref list.m_currentOp, (int)ListOperation.Take);
#pragma warning restore 0420
                    //Synchronization cases:
                    // if the list count is less than or equal two to avoid conflict with any stealing thread
                    // if m_needSync is set, this means there is a thread that needs to freeze the bag
                    if (list.Count <= 2 || m_needSync)
                    {
                        // reset it back to zero to avoid deadlock with stealing thread
                        list.m_currentOp = (int)ListOperation.None;
                        Monitor.Enter(list, ref lockTaken);
  
                        // Double check the count and steal if it became empty
                        if (list.Count == 0)
                        {
                            // Release the lock before stealing
                            if (lockTaken)
                            {
                                try { }
                                finally
                                {
                                    lockTaken = false; // reset lockTaken to avoid calling Monitor.Exit again in the finally block
                                    Monitor.Exit(list);
                                }
                            }
                            return Steal(out result, true);
                        }
                    }
                    list.Remove(out result);
                }
                else
                {
                    if (!list.Peek(out result))
                    {
                        return Steal(out result, false);
                    }
                }
            }
            finally
            {
                list.m_currentOp = (int)ListOperation.None;
                if (lockTaken)
                {
                    Monitor.Exit(list);
                }
            }
            return true;
        }
 
 
        /// <summary>
        /// Local helper function to retrieve a thread local list by a thread object
        /// </summary>
        /// Create a new list if the thread does ot exist
        /// <returns>The local list object</returns>
        private ThreadLocalList<T> GetThreadList(bool forceCreate)
        {
            ThreadLocalList<T> list = m_locals.Value;
 
            if (list != null)
            {
                return list;
            }
            else if (forceCreate)
            {
                // Acquire the lock to update the m_tailList pointer
                lock (m_globalListsLock)
                {
                    if (m_headList == null)
                    {
                        list = new ThreadLocalList<T>(Thread.CurrentThread);
                        m_headList = list;
                        m_tailList = list;
                    }
                    else
                    {
 
                        list = GetUnownedList();
                        if (list == null)
                        {
                            list = new ThreadLocalList<T>(Thread.CurrentThread);
                            m_tailList.m_nextList = list;
                            m_tailList = list;
                        }
                    }
                    m_locals.Value = list;
                }
            }
            else
            {
                return null;
            }
            Debug.Assert(list != null);
            return list;
 
        }
  
        /// <summary>
        /// Try to reuse an unowned list if exist
        /// unowned lists are the lists that their owner threads are aborted or terminated
        /// this is workaround to avoid memory leaks.
        /// </summary>
        /// <returns>The list object, null if all lists are owned</returns>
        private ThreadLocalList<T> GetUnownedList()
        {
            ThreadLocalList<T> currentList = m_headList;
            while (currentList != null)
            {
                if (currentList.m_ownerThread.ThreadState == System.Threading.ThreadState.Stopped)
                {
                    currentList.m_ownerThread = Thread.CurrentThread; // the caller should acquire a lock to make this line thread safe
                    return currentList;
                }
                currentList = currentList.m_nextList;
            }
            return null;
        }
  
 
        /// <summary>
        /// Local helper method to steal an item from any other non empty thread
        /// It enumerate all other threads in two passes first pass acquire the lock with TryEnter if succeeded
        /// it steals the item, otherwise it enumerate them again in 2nd pass and acquire the lock using Enter
        /// </summary>
        /// To receive the item retrieved from the bag
        /// Whether to remove or peek.
        /// <returns>True if succeeded, false otherwise.</returns>
        private bool Steal(out T result, bool take)
        {
            bool loop;
            do
            {
                loop = false;
                List<int> versionsList = new List<int>(); // save the lists version
  
                ThreadLocalList<T> currentList = m_headList;
                while (currentList != null)
                {
                    versionsList.Add(currentList.m_version);
                    if (currentList.m_head != null && TrySteal(currentList, out result, take))
                    {
                        return true;
                    }
                    currentList = currentList.m_nextList;
                }
  
                // verify versioning, if other items are added to this list since we last visit it, we should retry
                currentList = m_headList;
                foreach (int version in versionsList)
                {
                    if (version != currentList.m_version) //oops state changed
                    {
                        loop = true;
                        if (currentList.m_head != null && TrySteal(currentList, out result, take))
                            return true;
                    }
                    currentList = currentList.m_nextList;
                }
            } while (loop);
 
 
            result = default(T);
            return false;
        }
  
        /// <summary>
        /// local helper function tries to steal an item from given local list
        /// </summary>
        private bool TrySteal(ThreadLocalList<T> list, out T result, bool take)
        {
            lock (list)
            {
                if (CanSteal(list))
                {
                    list.Steal(out result, take);
                    return true;
                }
                result = default(T);
                return false;
            }
 
        }
        /// <summary>
        /// Local helper function to check the list if it became empty after acquiring the lock
        /// and wait if there is unsynchronized Add/Take operation in the list to be done
        /// </summary>
        /// The list to steal
        /// <returns>True if can steal, false otherwise</returns>
        private bool CanSteal(ThreadLocalList<T> list)
        {
            if (list.Count <= 2 && list.m_currentOp != (int)ListOperation.None)
            {
                SpinWait spinner = new SpinWait();
                while (list.m_currentOp != (int)ListOperation.None)
                {
                    spinner.SpinOnce();
                }
            }
            if (list.Count > 0)
            {
                return true;
            }
            return false;
        }
 
        /// <summary>
        /// Copies the <see cref="ConcurrentBag{T}"> elements to an existing
        /// one-dimensional <see cref="T:System.Array">Array</see>, starting at the specified array
        /// index.
        /// </see></summary>
        /// The one-dimensional <see cref="T:System.Array">Array</see> that is the
        /// destination of the elements copied from the
        /// <see cref="ConcurrentBag{T}">. The <see cref="T:System.Array">Array</see> must have zero-based indexing.
        /// The zero-based index in <paramref name="array"> at which copying
        /// begins.
        /// <exception cref="ArgumentNullException"><paramref name="array"> is a null reference (Nothing in
        /// Visual Basic).</paramref></exception>
        /// <exception cref="ArgumentOutOfRangeException"><paramref name="index"> is less than
        /// zero.</paramref></exception>
        /// <exception cref="ArgumentException"><paramref name="index"> is equal to or greater than the
        /// length of the <paramref name="array">
        /// -or- the number of elements in the source <see cref="ConcurrentBag{T}"> is greater than the available space from
        /// <paramref name="index"> to the end of the destination <paramref name="array">.</paramref></paramref></see></paramref></paramref></exception>
        public void CopyTo(T[] array, int index)
        {
            if (array == null)
            {
                throw new ArgumentNullException("array", "SR.ConcurrentBag_CopyTo_ArgumentNullException");
            }
            if (index < 0)
            {
                throw new ArgumentOutOfRangeException
                    ("index", "SR.ConcurrentBag_CopyTo_ArgumentOutOfRangeException");
            }
  
            // Short path if the bag is empty
            if (m_headList == null)
                return;
 
            bool lockTaken = false;
            try
            {
                FreezeBag(ref lockTaken);
                ToList().CopyTo(array, index);
            }
            finally
            {
                UnfreezeBag(lockTaken);
            }
        }
 
        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.ICollection"> to an <see cref="T:System.Array">, starting at a particular
        /// <see cref="T:System.Array"> index.
        /// </see></see></see></summary>
        /// The one-dimensional <see cref="T:System.Array">Array</see> that is the
        /// destination of the elements copied from the
        /// <see cref="ConcurrentBag{T}">. The <see cref="T:System.Array">Array</see> must have zero-based indexing.
        /// The zero-based index in <paramref name="array"> at which copying
        /// begins.
        /// <exception cref="ArgumentNullException"><paramref name="array"> is a null reference (Nothing in
        /// Visual Basic).</paramref></exception>
        /// <exception cref="ArgumentOutOfRangeException"><paramref name="index"> is less than
        /// zero.</paramref></exception>
        /// <exception cref="ArgumentException">
        /// <paramref name="array"> is multidimensional. -or-
        /// <paramref name="array"> does not have zero-based indexing. -or-
        /// <paramref name="index"> is equal to or greater than the length of the <paramref name="array">
        /// -or- The number of elements in the source <see cref="T:System.Collections.ICollection"> is
        /// greater than the available space from <paramref name="index"> to the end of the destination
        /// <paramref name="array">. -or- The type of the source <see cref="T:System.Collections.ICollection"> cannot be cast automatically to the type of the
        /// destination <paramref name="array">.
        /// </paramref></see></paramref></paramref></see></paramref></paramref></paramref></paramref></exception>
        void ICollection.CopyTo(Array array, int index)
        {
            if (array == null)
            {
                throw new ArgumentNullException("array", "SR.ConcurrentBag_CopyTo_ArgumentNullException");
            }
 
            bool lockTaken = false;
            try
            {
                FreezeBag(ref lockTaken);
                ((ICollection)ToList()).CopyTo(array, index);
            }
            finally
            {
                UnfreezeBag(lockTaken);
            }
 
        }
 
  
        /// <summary>
        /// Copies the <see cref="ConcurrentBag{T}"> elements to a new array.
        /// </see></summary>
        /// <returns>A new array containing a snapshot of elements copied from the <see cref="ConcurrentBag{T}">.</see></returns>
        public T[] ToArray()
        {
            // Short path if the bag is empty
            if (m_headList == null)
                return new T[0];
 
            bool lockTaken = false;
            try
            {
                FreezeBag(ref lockTaken);
                return ToList().ToArray();
            }
            finally
            {
                UnfreezeBag(lockTaken);
            }
        }
 
        /// <summary>
        /// Returns an enumerator that iterates through the <see cref="ConcurrentBag{T}">.
        /// </see></summary>
        /// <returns>An enumerator for the contents of the <see cref="ConcurrentBag{T}">.</see></returns>
        /// <remarks>
        /// The enumeration represents a moment-in-time snapshot of the contents
        /// of the bag.  It does not reflect any updates to the collection after
        /// <see cref="GetEnumerator"> was called.  The enumerator is safe to use
        /// concurrently with reads from and writes to the bag.
        /// </see></remarks>
        public IEnumerator<T> GetEnumerator()
        {
            // CopyTo the items to a new array and enumerate the array
            T[] array = ToArray();
            return ((IEnumerable<T>)array).GetEnumerator();
        }
 
        /// <summary>
        /// Returns an enumerator that iterates through the <see cref="ConcurrentBag{T}">.
        /// </see></summary>
        /// <returns>An enumerator for the contents of the <see cref="ConcurrentBag{T}">.</see></returns>
        /// <remarks>
        /// The items enumerated represent a moment-in-time snapshot of the contents
        /// of the bag.  It does not reflect any update to the collection after
        /// <see cref="GetEnumerator"> was called.
        /// </see></remarks>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((MyConcurrentBag<T>)this).GetEnumerator();
        }
 
        /// <summary>
        /// Get the data array to be serialized
        /// </summary>
        [OnSerializing]
        private void OnSerializing(StreamingContext context)
        {
            // save the data into the serialization array to be saved
            m_serializationArray = ToArray();
        }
 
        /// <summary>
        /// Construct the stack from a previously seiralized one
        /// </summary>
        [OnDeserialized]
        private void OnDeserialized(StreamingContext context)
        {
            m_locals = new ThreadLocal<ThreadLocalList<T>>();
            m_globalListsLock = new object();
 
            ThreadLocalList<T> list = GetThreadList(true);
            foreach (T item in m_serializationArray)
            {
                AddInternal(list, item);
            }
            m_headList = list;
            m_tailList = list;
 
            m_serializationArray = null;
        }
  
        /// <summary>
        /// Gets the number of elements contained in the <see cref="ConcurrentBag{T}">.
        /// </see></summary>
        /// <value>The number of elements contained in the <see cref="ConcurrentBag{T}">.</see></value>
        /// <remarks>
        /// The count returned represents a moment-in-time snapshot of the contents
        /// of the bag.  It does not reflect any updates to the collection after
        /// <see cref="GetEnumerator"> was called.
        /// </see></remarks>
        public int Count
        {
            get
            {
                // Short path if the bag is empty
                if (m_headList == null)
                    return 0;
 
                bool lockTaken = false;
                try
                {
                    FreezeBag(ref lockTaken);
                    return GetCountInternal();
                }
                finally
                {
                    UnfreezeBag(lockTaken);
                }
            }
        }
  
        /// <summary>
        /// Gets a value that indicates whether the <see cref="ConcurrentBag{T}"> is empty.
        /// </see></summary>
        /// <value>true if the <see cref="ConcurrentBag{T}"> is empty; otherwise, false.</see></value>
        public bool IsEmpty
        {
            get
            {
                if (m_headList == null)
                    return true;
  
                bool lockTaken = false;
                try
                {
                    FreezeBag(ref lockTaken);
                    ThreadLocalList<T> currentList = m_headList;
                    while (currentList != null)
                    {
                        if (currentList.m_head != null)
                            //at least this list is not empty, we return false
                        {
                            return false;
                        }
                        currentList = currentList.m_nextList;
                    }
                    return true;
                }
                finally
                {
                    UnfreezeBag(lockTaken);
                }
            }
        }
 
        /// <summary>
        /// Gets a value indicating whether access to the <see cref="T:System.Collections.ICollection"> is
        /// synchronized with the SyncRoot.
        /// </see></summary>
        /// <value>true if access to the <see cref="T:System.Collections.ICollection"> is synchronized
        /// with the SyncRoot; otherwise, false. For <see cref="ConcurrentBag{T}">, this property always
        /// returns false.</see></see></value>
        bool ICollection.IsSynchronized
        {
            get { return false; }
        }
 
        /// <summary>
        /// Gets an object that can be used to synchronize access to the <see cref="T:System.Collections.ICollection">. This property is not supported.
        /// </see></summary>
        /// <exception cref="T:System.NotSupportedException">The SyncRoot property is not supported.</exception>
        object ICollection.SyncRoot
        {
            get
            {
                throw new NotSupportedException("SR.ConcurrentCollection_SyncRoot_NotSupported");
            }
        }
 
  
        #region Freeze bag helper methods
        /// <summary>
        /// Local helper method to freeze all bag operations, it
        /// 1- Acquire the global lock to prevent any other thread to freeze the bag, and also new new thread can be added
        /// to the dictionary
        /// 2- Then Acquire all local lists locks to prevent steal and synchronized operations
        /// 3- Wait for all un-synchronized operations to be done
        /// </summary>
        /// Retrieve the lock taken result for the global lock, to be passed to Unfreeze method
        private void FreezeBag(ref bool lockTaken)
        {
            // global lock to be safe against multi threads calls count and corrupt m_needSync
            Monitor.Enter(m_globalListsLock, ref lockTaken);
  
            // This will force any future add/take operation to be synchronized
            m_needSync = true;
 
            //Acquire all local lists locks
            AcquireAllLocks();
  
            // Wait for all un-synchronized operation to be done
            WaitAllOperations();
        }
 
        /// <summary>
        /// Local helper method to unfreeze the bag from a frozen state
        /// </summary>
        /// The lock taken result from the Freeze method
        private void UnfreezeBag(bool lockTaken)
        {
 
            ReleaseAllLocks();
            m_needSync = false;
            if (lockTaken)
            {
                Monitor.Exit(m_globalListsLock);
            }
        }
  
        /// <summary>
        /// local helper method to acquire all local lists locks
        /// </summary>
        private void AcquireAllLocks()
        {
  
            bool lockTaken = false;
            ThreadLocalList<T> currentList = m_headList;
            while (currentList != null)
            {
                // Try/Finally bllock to avoid thread aport between acquiring the lock and setting the taken flag
                try
                {
                    Monitor.Enter(currentList, ref lockTaken);
                }
                finally
                {
                    if (lockTaken)
                    {
                        currentList.m_lockTaken = true;
                        lockTaken = false;
                    }
                }
                currentList = currentList.m_nextList;
            }
        }
  
        /// <summary>
        /// Local helper method to release all local lists locks
        /// </summary>
        private void ReleaseAllLocks()
        {
            ThreadLocalList<T> currentList = m_headList;
            while (currentList != null)
            {
  
                if (currentList.m_lockTaken)
                {
                    currentList.m_lockTaken = false;
                    Monitor.Exit(currentList);
                }
                currentList = currentList.m_nextList;
            }
        }
  
        /// <summary>
        /// Local helper function to wait all unsynchronized operations
        /// </summary>
        private void WaitAllOperations()
        {
            ThreadLocalList<T> currentList = m_headList;
            while (currentList != null)
            {
                if (currentList.m_currentOp != (int)ListOperation.None)
                {
                    SpinWait spinner = new SpinWait();
                    while (currentList.m_currentOp != (int)ListOperation.None)
                    {
                        spinner.SpinOnce();
                    }
                }
                currentList = currentList.m_nextList;
            }
        }
  
        /// <summary>
        /// Local helper function to get the bag count, the caller should call it from Freeze/Unfreeze block
        /// </summary>
        /// <returns>The current bag count</returns>
        private int GetCountInternal()
        {
            int count = 0;
            ThreadLocalList<T> currentList = m_headList;
            while (currentList != null)
            {
                checked
                {
                    count += currentList.Count;
                }
                currentList = currentList.m_nextList;
            }
            return count;
        }
 
        /// <summary>
        /// Local helper function to return the bag item in a list, this is mainly used by CopyTo and ToArray
        /// This is not thread safe, should be called in Freeze/UnFreeze bag block
        /// </summary>
        /// <returns>List the contains the bag items</returns>
        public List<T> ToList()
        {
            List<T> list = new List<T>();
            ThreadLocalList<T> currentList = m_headList;
            while (currentList != null)
            {
                Node<T> currentNode = currentList.m_head;
                while (currentNode != null)
                {
                    list.Add(currentNode.m_value);
                    currentNode = currentNode.m_next;
                }
                currentList = currentList.m_nextList;
            }
 
            return list;
        }

        public List<T> TakeAll()
        {
            List<T> list = new List<T>();
            ThreadLocalList<T> currentList = m_headList;
            while (currentList != null)
            {
                Node<T> currentNode = currentList.m_head;
                while (currentNode != null)
                {
                    list.Add(currentNode.m_value);
                    currentNode = currentNode.m_next;
                }
                currentList.Flush();
                currentList = currentList.m_nextList;
            }

            return list;
        }
  
        #endregion
       
    }
  
}
 
// File provided for Reference Use Only by Microsoft Corporation (c) 2007.
﻿